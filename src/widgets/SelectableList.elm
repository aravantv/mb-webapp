module SelectableList exposing (..)

import Binding exposing (ListBinding)
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import ListUtils exposing (..)
import MetaModel exposing (ModelElementIdentifier, getItemIdentifier)
import Model exposing (Model)
import Utils exposing (enterKey, onKeyDown, onKeyUp, shiftCode, tabKey)
import Widget exposing (Factory, IDecision, ISelectable, Index, UnboundWidget, Widget, cmdOfMsg, doNothing)


type alias ItemWidget model msg =
    ISelectable model msg (Widget model msg)


type alias NewItemWidget model msg =
    IDecision msg (UnboundWidget model msg)


type alias Parameters newItemModel itemModel newItemMsg itemMsg =
    { binding : ListBinding (Msg newItemMsg itemMsg itemModel)
    , newItemWidget : NewItemWidget newItemModel newItemMsg
    , itemWidget : ItemWidget itemModel itemMsg
    , factory : Factory newItemModel
    }


createWidget :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> Widget (Model newItemModel itemModel) (Msg newItemMsg itemMsg itemModel)
createWidget params =
    { initModel = emptyModel params
    , initMsg = Init
    , update = update params
    , subscriptions = subscriptions params
    , view = view params
    }



-- MODEL


type alias Model newItemModel itemModel =
    { itemToAdd : newItemModel, contents : List itemModel }


emptyModel : Parameters newItemModel itemModel newItemMsg itemMsg -> Model newItemModel itemModel
emptyModel params =
    { itemToAdd = params.newItemWidget.initModel, contents = [] }



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
    -> Msg newItemMsg itemMsg itemModel
    -> Model newItemModel itemModel
    -> ModelElementIdentifier
    -> ( Model newItemModel itemModel, Cmd (Msg newItemMsg itemMsg itemModel) )
update params msg model id =
    case msg of
        DelegateToNewItemMsg subMsg ->
            if subMsg == params.newItemWidget.confirmMsg then
                ( { model | itemToAdd = params.newItemWidget.initModel }
                , addItemCmd params id 0 (params.factory model.itemToAdd)
                )
            else
                delegateUpdateToItemToAdd params model subMsg

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
            update params (DelegateToItemMsg (i + 1) params.itemWidget.selectMsg) model id

        SelectPrevious i ->
            update params (DelegateToItemMsg (i - 1) params.itemWidget.selectMsg) model id

        BackendAddedItem ( i, m ) ->
            case insert model.contents params.itemWidget.initModel i of
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
            ( emptyModel params, Cmd.none )

        NoOp ->
            doNothing model


delegateUpdateToItemToAdd :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> Model newItemModel itemModel
    -> newItemMsg
    -> ( Model newItemModel itemModel, Cmd (Msg newItemMsg itemMsg itemModel) )
delegateUpdateToItemToAdd params model subMsg =
    let
        ( updatedItemToAdd, cmd ) =
            params.newItemWidget.update subMsg model.itemToAdd
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
            cmdOfMsg (DelegateToItemMsg indexToAdd (params.itemWidget.initMsg itemToAdd))
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
                params.itemWidget.update params.itemWidget.unselectMsg itemModel (getItemIdentifier id j)
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
                ( updatedSubModel, cmd ) =
                    params.itemWidget.update itemMsg subModel (MetaModel.getItemIdentifier id itemIndex)
            in
                ( List.take itemIndex contents ++ [ updatedSubModel ] ++ List.drop (itemIndex + 1) contents
                , Cmd.map (DelegateToItemMsg itemIndex) cmd
                )



-- SUBSCRIPTIONS


subscriptions :
    Parameters newItemModel itemModel newItemMsg itemMsg
    -> Model newItemModel itemModel
    -> ModelElementIdentifier
    -> Sub (Msg newItemMsg itemMsg itemModel)
subscriptions params model id =
    let
        itemSub i itemModel =
            Sub.map (DelegateToItemMsg i) <| params.itemWidget.subscriptions itemModel (params.binding.getChildIdentifier id i)

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
    -> Model newItemModel itemModel
    -> Html (Msg newItemMsg itemMsg itemModel)
view params model =
    let
        delegateViewToItem i m =
            li []
                [ span
                    {--We need to use key down here because browsers have their own interpretation of this key combination. --}
                    [ onKeyDown [ ( tabKey, SelectNext i ), ( shiftCode tabKey, SelectPrevious i ) ] ]
                    [ Html.map (DelegateToItemMsg i) <| params.itemWidget.view m
                    , button [ onClick <| Remove i ] [ text "-" ]
                    ]
                ]
    in
        ul [] <|
            li [] [ Html.map DelegateToNewItemMsg <| params.newItemWidget.view model.itemToAdd ]
                :: List.indexedMap delegateViewToItem model.contents
