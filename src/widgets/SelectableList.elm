module SelectableList exposing (..)

import Binding exposing (ListBinding)
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import ListUtils exposing (..)
import MetaModel exposing (ModelElementIdentifier, getItemIdentifier)
import Model exposing (Model)
import Utils exposing (enterKey, onKeyDown, onKeyUp, shiftCode, tabKey)
import Widget exposing (BoundWidget, Factory, IDecision, ISelectable, Index, Unbound, Widget, cmdOfMsg, doNothing)


type alias ItemWidget model msg =
    ISelectable model msg { widget : Widget model msg }


type alias NewItemWidget model msg =
    IDecision msg { widget : Widget model msg }


type alias Parameters newItemModel itemModel newItemMsg itemMsg =
    { binding : ListBinding (Msg newItemMsg itemMsg itemModel)
    , newItemWidget : NewItemWidget newItemModel newItemMsg
    , itemWidget : ItemWidget itemModel itemMsg
    , factory : Factory newItemModel
    }


createWidget :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> Widget (Model newItemModel itemModel) (Msg newItemMsg itemMsg itemModel)
createWidget params id =
    { initModel = emptyModel params id
    , initMsg = Init
    , update = update params id
    , subscriptions = subscriptions params id
    , view = view params id
    }



-- MODEL


type alias Model newItemModel itemModel =
    { itemToAdd : newItemModel, contents : List itemModel }


emptyModel :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> ModelElementIdentifier
    -> Model newItemModel itemModel
emptyModel params id =
    { itemToAdd = (params.newItemWidget.widget id).initModel, contents = [] }



-- UPDATE


type Msg newItemMsg itemMsg itemModel
    = DelegateToNewItemMsg newItemMsg
    | DelegateToItemMsg Index itemMsg
    | Remove Index
    | SelectNext Index
    | SelectPrevious Index
    | BackendAddedItem ( Index, Model.Model )
    | BackendRemovedItem Index
    | NoOp
    | Init Model.Model


update :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> ModelElementIdentifier
    -> Msg newItemMsg itemMsg itemModel
    -> Model newItemModel itemModel
    -> ( Model newItemModel itemModel, Cmd (Msg newItemMsg itemMsg itemModel) )
update params id msg model =
    case msg of
        DelegateToNewItemMsg subMsg ->
            if subMsg == params.newItemWidget.confirmMsg then
                ( { model | itemToAdd = (params.newItemWidget.widget id).initModel }
                , addItemCmd params id 0 (params.factory model.itemToAdd)
                )
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
                ( { model | contents = unselectedUpdatedItems }, Cmd.batch [ updateCmd, unselectCmds ] )

        Remove i ->
            case params.binding.removeItem id i of
                Binding.Ok cmd ->
                    ( model, cmd )

                Binding.Err _ ->
                    doNothing model

                Binding.Irrelevant ->
                    doNothing model

        SelectNext i ->
            update params id (DelegateToItemMsg (i + 1) params.itemWidget.selectMsg) model

        SelectPrevious i ->
            update params id (DelegateToItemMsg (i - 1) params.itemWidget.selectMsg) model

        BackendAddedItem ( i, m ) ->
            case insert model.contents (params.itemWidget.widget id).initModel i of
                Just newContents ->
                    ( { model | contents = newContents }, params.binding.askItemContent id i )

                Nothing ->
                    doNothing model

        BackendRemovedItem i ->
            doNothing <|
                case remove model.contents i of
                    Just newContents ->
                        { model | contents = newContents }

                    Nothing ->
                        model

        Init l ->
            ( emptyModel params id, Cmd.none )

        NoOp ->
            doNothing model


delegateUpdateToItemToAdd :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> ModelElementIdentifier
    -> Model newItemModel itemModel
    -> newItemMsg
    -> ( Model newItemModel itemModel, Cmd (Msg newItemMsg itemMsg itemModel) )
delegateUpdateToItemToAdd params id model subMsg =
    let
        ( updatedItemToAdd, cmd ) =
            (params.newItemWidget.widget id).update subMsg model.itemToAdd
    in
        ( { model | itemToAdd = updatedItemToAdd }, Cmd.map DelegateToNewItemMsg cmd )


addItemCmd :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> ModelElementIdentifier
    -> Index
    -> Model.Model
    -> Cmd (Msg newItemMsg itemMsg itemModel)
addItemCmd params id indexToAdd itemToAdd =
    let
        initItemWithItemToAddCmd =
            cmdOfMsg (DelegateToItemMsg indexToAdd ((params.itemWidget.widget id).initMsg itemToAdd))
    in
        case params.binding.addItem id indexToAdd itemToAdd of
            Binding.Ok res ->
                Cmd.batch [ res, initItemWithItemToAddCmd ]

            Binding.Err _ ->
                initItemWithItemToAddCmd

            Binding.Irrelevant ->
                initItemWithItemToAddCmd


unselectPreviouslySelectedItems :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> ModelElementIdentifier
    -> List itemModel
    -> Index
    -> itemMsg
    -> ( List itemModel, Cmd (Msg newItemMsg itemMsg itemModel) )
unselectPreviouslySelectedItems params id items exceptIndex msg =
    let
        unselectIfPreviouslySelected j itemModel =
            if params.itemWidget.isSelected itemModel && j /= exceptIndex then
                let
                    instantiatedWidget =
                        params.itemWidget.widget (getItemIdentifier id j)
                in
                    instantiatedWidget.update params.itemWidget.unselectMsg itemModel
            else
                doNothing itemModel

        ( unselectedItems, unselectCmds ) =
            List.unzip (List.indexedMap unselectIfPreviouslySelected items)

        finalCmd =
            Cmd.batch <| List.indexedMap (\i -> Cmd.map (DelegateToItemMsg i)) unselectCmds
    in
        ( unselectedItems, finalCmd )


delegateUpdateToItem :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> ModelElementIdentifier
    -> List itemModel
    -> Index
    -> itemMsg
    -> ( List itemModel, Cmd (Msg newItemMsg itemMsg itemModel) )
delegateUpdateToItem params id contents itemIndex itemMsg =
    case get contents itemIndex of
        Nothing ->
            doNothing contents

        Just subModel ->
            let
                instantiatedWidget =
                    params.itemWidget.widget (MetaModel.getItemIdentifier id itemIndex)

                ( updatedSubModel, cmd ) =
                    instantiatedWidget.update itemMsg subModel
            in
                ( List.take itemIndex contents ++ [ updatedSubModel ] ++ List.drop (itemIndex + 1) contents
                , Cmd.map (DelegateToItemMsg itemIndex) cmd
                )



-- SUBSCRIPTIONS


subscriptions :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> ModelElementIdentifier
    -> Model newItemModel itemModel
    -> Sub (Msg newItemMsg itemMsg itemModel)
subscriptions params id model =
    let
        itemSub i itemModel =
            let
                instantiatedWidget =
                    params.itemWidget.widget (params.binding.getChildIdentifier id i)
            in
                Sub.map (DelegateToItemMsg i) (instantiatedWidget.subscriptions itemModel)

        msgOfBindingRes f res =
            case res of
                Binding.Ok v ->
                    f v

                Binding.Irrelevant ->
                    NoOp

                {--No handling of error for now: no use case actually...--}
                Binding.Err err ->
                    NoOp
    in
        Sub.batch
            ([ Sub.map (msgOfBindingRes BackendAddedItem) (params.binding.itemAdded id)
             , Sub.map (msgOfBindingRes BackendRemovedItem) (params.binding.itemRemoved id)
             ]
                ++ List.indexedMap itemSub model.contents
            )



-- VIEW


view :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> ModelElementIdentifier
    -> Model newItemModel itemModel
    -> Html (Msg newItemMsg itemMsg itemModel)
view params id model =
    let
        instantiatedWidget i =
            params.itemWidget.widget (params.binding.getChildIdentifier id i)

        delegateViewToItem i m =
            li []
                [ span
                    {--We need to use key down here because browsers have their own interpretation of this key combination. --}
                    [ onKeyDown [ ( tabKey, SelectNext i ), ( shiftCode tabKey, SelectPrevious i ) ] ]
                    [ Html.map (DelegateToItemMsg i) <| (instantiatedWidget i).view m
                    , button [ onClick <| Remove i ] [ text "-" ]
                    ]
                ]

        instantiatedNewWidget =
            params.newItemWidget.widget id
    in
        ul [] <|
            li [] [ Html.map DelegateToNewItemMsg <| instantiatedNewWidget.view model.itemToAdd ]
                :: List.indexedMap delegateViewToItem model.contents
