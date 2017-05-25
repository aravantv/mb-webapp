module SelectableList exposing (..)

import Binding exposing (Binding)
import CollectionBinding exposing (BoundCollectionWidget, CollectionBindingSubInfo, CollectionBindingUpInfo, CollectionBindingUpInfo(..), doNothing)
import Data exposing (Data)
import DataID exposing (DataID, getItemIdentifier)
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import ListUtils exposing (..)
import Utils exposing (enterKey, onKeyDown, onKeyUp, shiftCode, tabKey)
import Widget exposing (Factory, IDecision, ISelectable, Index, Widget, cmdOf, cmdOfMsg, modelOf)


type alias ItemWidget model msg =
    ISelectable model msg { makeWidget : DataID -> Widget () () model msg }


type alias NewItemWidget model msg =
    IDecision msg { widget : Widget () () model msg }


type alias Parameters newItemModel itemModel newItemMsg itemMsg =
    { newItemWidget : NewItemWidget newItemModel newItemMsg
    , itemWidget : ItemWidget itemModel itemMsg
    , factory : Factory newItemModel
    }


createWidget :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> BoundCollectionWidget Index (Model newItemModel itemModel) (Msg newItemMsg itemMsg itemModel) Data
createWidget params =
    { init = init params
    , update = update params
    , subscriptions = subscriptions params
    , view = view params
    }



-- MODEL


type alias ModelWithDataID itemModel =
    { model : itemModel, id : DataID }


type alias Contents itemModel =
    List (ModelWithDataID itemModel)


type alias Model newItemModel itemModel =
    { itemToAdd : newItemModel
    , contents : Contents itemModel
    }


init :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> ( Model newItemModel itemModel, Cmd (Msg newItemMsg itemMsg itemModel) )
init params =
    ( { itemToAdd = modelOf params.newItemWidget.widget.init, contents = [] }
    , Cmd.map DelegateToNewItemMsg (cmdOf params.newItemWidget.widget.init)
    )



-- UPDATE


type Msg newItemMsg itemMsg itemModel
    = DelegateToNewItemMsg newItemMsg
    | DelegateToItemMsg ( Index, itemMsg )
    | Remove Index
    | SelectNext Index
    | SelectPrevious Index
    | BackendAddedItem ( Index, DataID )
    | BackendRemovedItem Index
    | NoOp


update :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> Msg newItemMsg itemMsg itemModel
    -> Model newItemModel itemModel
    -> ( Model newItemModel itemModel, Cmd (Msg newItemMsg itemMsg itemModel), CollectionBindingUpInfo Index Data )
update params msg model =
    case msg of
        Remove i ->
            ( model, Cmd.none, RemoveItem (Binding.Ok i) )

        SelectNext i ->
            update params (DelegateToItemMsg ( i + 1, params.itemWidget.selectMsg )) model

        SelectPrevious i ->
            update params (DelegateToItemMsg ( i - 1, params.itemWidget.selectMsg )) model

        DelegateToItemMsg ( i, subMsg ) ->
            let
                ( updatedItems, updateCmd ) =
                    delegateUpdateToItem params model.contents ( i, subMsg )

                ( possiblyUnselectedItems, unselectCmds ) =
                    if subMsg == params.itemWidget.selectMsg then
                        unselectPreviouslySelectedItems params updatedItems i
                    else
                        ( updatedItems, Cmd.none )
            in
                ( { model | contents = possiblyUnselectedItems }, Cmd.batch [ updateCmd, unselectCmds ], DoNothing )

        DelegateToNewItemMsg subMsg ->
            if subMsg == params.newItemWidget.confirmMsg then
                ( { model | itemToAdd = modelOf params.newItemWidget.widget.init }
                , Cmd.map DelegateToNewItemMsg (cmdOf params.newItemWidget.widget.init)
                , CollectionBinding.AddItem (Binding.Ok ( 0, params.factory model.itemToAdd ))
                )
            else
                let
                    ( updatedItemToAdd, cmd, () ) =
                        params.newItemWidget.widget.update subMsg model.itemToAdd
                in
                    ( { model | itemToAdd = updatedItemToAdd }, Cmd.map DelegateToNewItemMsg cmd, DoNothing )

        BackendAddedItem ( i, itemID ) ->
            let
                ( addedItemModel, addedItemCmd ) =
                    (params.itemWidget.makeWidget itemID).init
            in
                doNothing <|
                    case insert model.contents { model = addedItemModel, id = itemID } i of
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

        NoOp ->
            doNothing model


unselectPreviouslySelectedItems :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> Contents itemModel
    -> Index
    -> ( Contents itemModel, Cmd (Msg newItemMsg itemMsg itemModel) )
unselectPreviouslySelectedItems params items exceptIndex =
    let
        unselectIfPreviouslySelected j modelWithID =
            if params.itemWidget.isSelected modelWithID.model && j /= exceptIndex then
                let
                    instantiatedWidget =
                        params.itemWidget.makeWidget modelWithID.id

                    ( newItemModel, cmd, () ) =
                        instantiatedWidget.update params.itemWidget.unselectMsg modelWithID.model
                in
                    ( { model = newItemModel, id = modelWithID.id }, cmd )
            else
                ( modelWithID, Cmd.none )

        ( unselectedItems, unselectCmds ) =
            List.unzip (List.indexedMap unselectIfPreviouslySelected items)

        finalCmd =
            Cmd.batch <| List.indexedMap (\i -> Cmd.map (\m -> DelegateToItemMsg ( i, m ))) unselectCmds
    in
        ( unselectedItems, finalCmd )


delegateUpdateToItem :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> Contents itemModel
    -> ( Index, itemMsg )
    -> ( Contents itemModel, Cmd (Msg newItemMsg itemMsg itemModel) )
delegateUpdateToItem params contents ( itemIndex, itemMsg ) =
    case get contents itemIndex of
        Nothing ->
            ( contents, Cmd.none )

        Just modelWithID ->
            let
                instantiatedWidget =
                    params.itemWidget.makeWidget modelWithID.id

                ( updatedSubModel, cmd, () ) =
                    instantiatedWidget.update itemMsg modelWithID.model
            in
                ( List.take itemIndex contents
                    ++ [ { model = updatedSubModel, id = modelWithID.id } ]
                    ++ List.drop (itemIndex + 1) contents
                , Cmd.map (\msg -> DelegateToItemMsg ( itemIndex, msg )) cmd
                )



-- SUBSCRIPTIONS


subscriptions :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> Model newItemModel itemModel
    -> ( Sub (Msg newItemMsg itemMsg itemModel), CollectionBindingSubInfo Index Data (Msg newItemMsg itemMsg itemModel) )
subscriptions params model =
    let
        itemSub i modelWithID =
            let
                ( subs, () ) =
                    (params.itemWidget.makeWidget modelWithID.id).subscriptions modelWithID.model
            in
                Sub.map (\m -> DelegateToItemMsg ( i, m )) subs
    in
        ( Sub.batch (List.indexedMap itemSub model.contents)
        , { itemAdded =
                \res ->
                    case res of
                        Binding.Ok ( p, _, id ) ->
                            BackendAddedItem ( p, id )

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
    -> Model newItemModel itemModel
    -> Html (Msg newItemMsg itemMsg itemModel)
view params model =
    let
        delegateViewToItem i modelWithID =
            li []
                [ span
                    {--We need to use key down here because browsers have their own interpretation of this key combination. --}
                    [ onKeyDown [ ( tabKey, SelectNext i ), ( shiftCode tabKey, SelectPrevious i ) ] ]
                    [ Html.map (\m -> DelegateToItemMsg ( i, m )) <|
                        (params.itemWidget.makeWidget modelWithID.id).view modelWithID.model
                    , button [ onClick <| Remove i ] [ text "-" ]
                    ]
                ]
    in
        ul [] <|
            li [] [ Html.map DelegateToNewItemMsg <| params.newItemWidget.widget.view model.itemToAdd ]
                :: List.indexedMap delegateViewToItem model.contents
