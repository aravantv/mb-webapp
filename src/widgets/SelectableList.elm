module SelectableList exposing (..)

import Binding exposing (Binding)
import CollectionBinding exposing (CollectionBindingSubInfo, CollectionBindingUpInfo, CollectionBindingUpInfo(..), WidgetWithCollectionBinding, doNothing)
import Data exposing (Data)
import DataID exposing (DataID, getItemIdentifier)
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import ListUtils exposing (..)
import Utils exposing (enterKey, onKeyDown, onKeyUp, shiftCode, tabKey)
import Widget exposing (BoundWidget, Factory, IDecision, ISelectable, Index, Unbound, Widget, cmdOfMsg)


type alias ItemWidget model msg =
    ISelectable model msg { widget : Widget () () model msg }


type alias NewItemWidget model msg =
    IDecision msg { widget : Widget () () model msg }


type alias Parameters newItemModel itemModel newItemMsg itemMsg =
    { newItemWidget : NewItemWidget newItemModel newItemMsg
    , itemWidget : ItemWidget itemModel itemMsg
    , factory : Factory newItemModel
    }


createWidget :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> WidgetWithCollectionBinding Index (Model newItemModel itemModel) (Msg newItemMsg itemMsg itemModel) Data
createWidget params id =
    { initModel = emptyModel params id
    , initMsg = Init
    , update = update params id
    , subscriptions = subscriptions params id
    , view = view params id
    }



-- MODEL


type alias Contents itemModel =
    List ( itemModel, DataID )


type alias Model newItemModel itemModel =
    { itemToAdd : newItemModel
    , contents : Contents itemModel
    }


emptyModel :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> DataID
    -> Model newItemModel itemModel
emptyModel params id =
    { itemToAdd = (params.newItemWidget.widget id).initModel
    , contents = []
    }



-- UPDATE


type Msg newItemMsg itemMsg itemModel
    = DelegateToNewItemMsg newItemMsg
    | DelegateToItemMsg Index itemMsg
    | Remove Index
    | SelectNext Index
    | SelectPrevious Index
    | BackendAddedItem ( Index, Data, DataID )
    | BackendRemovedItem Index
    | NoOp
    | Init Data


update :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> DataID
    -> Msg newItemMsg itemMsg itemModel
    -> Model newItemModel itemModel
    -> ( Model newItemModel itemModel, Cmd (Msg newItemMsg itemMsg itemModel), CollectionBindingUpInfo Index Data )
update params id msg model =
    case msg of
        DelegateToNewItemMsg subMsg ->
            let
                ( cmd, upInfo ) =
                    addItemCmd params id 0 (params.factory model.itemToAdd)
            in
                if subMsg == params.newItemWidget.confirmMsg then
                    ( { model | itemToAdd = (params.newItemWidget.widget id).initModel }, cmd, upInfo )
                else
                    delegateUpdateToItemToAdd params id model subMsg

        DelegateToItemMsg i subMsg ->
            let
                ( updatedItems, updateCmd ) =
                    delegateUpdateToItem params id model.contents i subMsg

                ( unselectedUpdatedItems, unselectCmds ) =
                    if subMsg == params.itemWidget.selectMsg then
                        unselectPreviouslySelectedItems params id updatedItems i subMsg
                    else
                        ( updatedItems, Cmd.none )
            in
                ( { model | contents = unselectedUpdatedItems }, Cmd.batch [ updateCmd, unselectCmds ], DoNothing )

        Remove i ->
            ( model, Cmd.none, RemoveItem (Binding.Ok i) )

        SelectNext i ->
            update params id (DelegateToItemMsg (i + 1) params.itemWidget.selectMsg) model

        SelectPrevious i ->
            update params id (DelegateToItemMsg (i - 1) params.itemWidget.selectMsg) model

        BackendAddedItem ( i, d, itemID ) ->
            let
                addedItemModel =
                    (params.itemWidget.widget itemID).initModel
            in
                doNothing <|
                    case insert model.contents ( addedItemModel, itemID ) i of
                        Just newContents ->
                            { model | contents = newContents }

                        Nothing ->
                            model

        BackendRemovedItem i ->
            doNothing <|
                case remove model.contents i of
                    Just newContents ->
                        { model | contents = newContents }

                    Nothing ->
                        model

        Init l ->
            ( emptyModel params id, Cmd.none, DoNothing )

        NoOp ->
            doNothing model


delegateUpdateToItemToAdd :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> DataID
    -> Model newItemModel itemModel
    -> newItemMsg
    -> ( Model newItemModel itemModel, Cmd (Msg newItemMsg itemMsg itemModel), CollectionBindingUpInfo Index carriedValue )
delegateUpdateToItemToAdd params id model subMsg =
    let
        ( updatedItemToAdd, cmd, () ) =
            (params.newItemWidget.widget id).update subMsg model.itemToAdd
    in
        ( { model | itemToAdd = updatedItemToAdd }, Cmd.map DelegateToNewItemMsg cmd, DoNothing )


addItemCmd :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> DataID
    -> Index
    -> Data
    -> ( Cmd (Msg newItemMsg itemMsg itemModel), CollectionBindingUpInfo Index Data )
addItemCmd params id indexToAdd itemToAdd =
    let
        initItemWithItemToAddCmd =
            cmdOfMsg (DelegateToItemMsg indexToAdd ((params.itemWidget.widget id).initMsg itemToAdd))
    in
        ( initItemWithItemToAddCmd, CollectionBinding.AddItem (Binding.Ok ( indexToAdd, itemToAdd )) )


unselectPreviouslySelectedItems :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> DataID
    -> Contents itemModel
    -> Index
    -> itemMsg
    -> ( Contents itemModel, Cmd (Msg newItemMsg itemMsg itemModel) )
unselectPreviouslySelectedItems params id items exceptIndex msg =
    let
        unselectIfPreviouslySelected j ( itemModel, itemID ) =
            if params.itemWidget.isSelected itemModel && j /= exceptIndex then
                let
                    instantiatedWidget =
                        params.itemWidget.widget itemID

                    ( newItemModel, cmd, () ) =
                        instantiatedWidget.update params.itemWidget.unselectMsg itemModel
                in
                    ( ( newItemModel, itemID ), cmd )
            else
                ( ( itemModel, itemID ), Cmd.none )

        ( unselectedItems, unselectCmds ) =
            List.unzip (List.indexedMap unselectIfPreviouslySelected items)

        finalCmd =
            Cmd.batch <| List.indexedMap (\i -> Cmd.map (DelegateToItemMsg i)) unselectCmds
    in
        ( unselectedItems, finalCmd )


delegateUpdateToItem :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> DataID
    -> Contents itemModel
    -> Index
    -> itemMsg
    -> ( Contents itemModel, Cmd (Msg newItemMsg itemMsg itemModel) )
delegateUpdateToItem params id contents itemIndex itemMsg =
    case get contents itemIndex of
        Nothing ->
            ( contents, Cmd.none )

        Just ( subModel, itemID ) ->
            let
                instantiatedWidget =
                    params.itemWidget.widget itemID

                ( updatedSubModel, cmd, () ) =
                    instantiatedWidget.update itemMsg subModel
            in
                ( List.take itemIndex contents ++ [ ( updatedSubModel, id ) ] ++ List.drop (itemIndex + 1) contents
                , Cmd.map (DelegateToItemMsg itemIndex) cmd
                )



-- SUBSCRIPTIONS


subscriptions :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> DataID
    -> Model newItemModel itemModel
    -> ( Sub (Msg newItemMsg itemMsg itemModel), CollectionBindingSubInfo Index Data (Msg newItemMsg itemMsg itemModel) )
subscriptions params id model =
    let
        itemSub i ( itemModel, itemID ) =
            let
                instantiatedWidget =
                    params.itemWidget.widget itemID

                ( subs, () ) =
                    instantiatedWidget.subscriptions itemModel
            in
                Sub.map (DelegateToItemMsg i) subs
    in
        ( Sub.batch (List.indexedMap itemSub model.contents)
        , { itemAdded =
                \res ->
                    case res of
                        Binding.Ok v ->
                            BackendAddedItem v

                        _ ->
                            NoOp
          , itemRemoved =
                \res ->
                    case res of
                        Binding.Ok v ->
                            BackendRemovedItem v

                        _ ->
                            NoOp
          }
        )



-- VIEW


view :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> DataID
    -> Model newItemModel itemModel
    -> Html (Msg newItemMsg itemMsg itemModel)
view params id model =
    let
        instantiatedWidget itemID =
            params.itemWidget.widget itemID

        delegateViewToItem i ( m, itemID ) =
            li []
                [ span
                    {--We need to use key down here because browsers have their own interpretation of this key combination. --}
                    [ onKeyDown [ ( tabKey, SelectNext i ), ( shiftCode tabKey, SelectPrevious i ) ] ]
                    [ Html.map (DelegateToItemMsg i) <| (instantiatedWidget itemID).view m
                    , button [ onClick <| Remove i ] [ text "-" ]
                    ]
                ]

        instantiatedNewWidget =
            params.newItemWidget.widget id
    in
        ul [] <|
            li [] [ Html.map DelegateToNewItemMsg <| instantiatedNewWidget.view model.itemToAdd ]
                :: List.indexedMap delegateViewToItem model.contents
