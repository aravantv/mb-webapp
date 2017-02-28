module SelectableList exposing (..)

import Binding exposing (ListBinding)
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import ListUtils exposing (..)
import Utils exposing (enterKey, onKeyDown, onKeyUp, shiftCode, tabKey)
import Widget exposing (IDecision, ISelectable, Index, Path, UnboundWidget, Widget, cmdOfMsg, doNothing)


type alias ItemWidget model msg factoryInput =
    ISelectable model msg (Widget model msg factoryInput)


type alias NewItemWidget model msg factoryInput =
    IDecision msg (UnboundWidget model msg factoryInput)


type alias Converter fromModel toModelFactoryInput =
    fromModel -> toModelFactoryInput


type alias Parameters newItemModel itemModel newItemMsg itemMsg factoryInput err =
    { binding : ListBinding (Msg newItemMsg itemMsg itemModel factoryInput) err
    , newItemWidget : NewItemWidget newItemModel newItemMsg factoryInput
    , itemWidget : ItemWidget itemModel itemMsg factoryInput
    , converter : Converter newItemModel factoryInput
    }


createWidget :
    Parameters newItemModel itemModel newItemMsg itemMsg factoryInput err
    -> Widget (Model newItemModel itemModel) (Msg newItemMsg itemMsg itemModel factoryInput) (List factoryInput)
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


emptyModel : Parameters newItemModel itemModel newItemMsg itemMsg factoryInput err -> Model newItemModel itemModel
emptyModel params =
    { itemToAdd = params.newItemWidget.initModel, contents = [] }



-- UPDATE


type Msg newItemMsg itemMsg itemModel factoryInput
    = DelegateToNewItemMsg newItemMsg
    | DelegateToItemMsg Index itemMsg
    | Remove Index
    | SelectNext Index
    | SelectPrevious Index
    | BackendAddedItem Index
    | BackendRemovedItem Index
    | NoOp
    | Init (List factoryInput)


update :
    Parameters newItemModel itemModel newItemMsg itemMsg factoryInput err
    -> Msg newItemMsg itemMsg itemModel factoryInput
    -> Model newItemModel itemModel
    -> Path
    -> ( Model newItemModel itemModel, Cmd (Msg newItemMsg itemMsg itemModel factoryInput) )
update params msg model path =
    case msg of
        DelegateToNewItemMsg subMsg ->
            if subMsg == params.newItemWidget.confirmMsg then
                ( { model | itemToAdd = params.newItemWidget.initModel }
                , addItemCmd params path 0 (params.converter model.itemToAdd)
                )
            else
                delegateUpdateToItemToAdd params model subMsg

        DelegateToItemMsg i subMsg ->
            let
                ( updatedItems, updateCmd ) =
                    delegateUpdateToItem params path model.contents i subMsg

                ( unselectedUpdatedItems, unselectCmds ) =
                    if subMsg == params.itemWidget.selectMsg then
                        unselectPreviouslySelectedItems params path updatedItems i subMsg
                    else
                        ( updatedItems, Cmd.none )
            in
                ( { model | contents = unselectedUpdatedItems }, Cmd.batch [ updateCmd, unselectCmds ] )

        Remove i ->
            ( model, params.binding.removeItem path i )

        SelectNext i ->
            update params (DelegateToItemMsg (i + 1) params.itemWidget.selectMsg) model path

        SelectPrevious i ->
            update params (DelegateToItemMsg (i - 1) params.itemWidget.selectMsg) model path

        BackendAddedItem i ->
            case insert model.contents params.itemWidget.initModel i of
                Just newContents ->
                    ( { model | contents = newContents }, params.binding.askItemContent path i )

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
            ( emptyModel params, Cmd.batch (List.indexedMap (\i fi -> addItemCmd params path i fi) l) )

        NoOp ->
            doNothing model


delegateUpdateToItemToAdd :
    Parameters newItemModel itemModel newItemMsg itemMsg factoryInput err
    -> Model newItemModel itemModel
    -> newItemMsg
    -> ( Model newItemModel itemModel, Cmd (Msg newItemMsg itemMsg itemModel factoryInput) )
delegateUpdateToItemToAdd params model subMsg =
    let
        ( updatedItemToAdd, cmd ) =
            params.newItemWidget.update subMsg model.itemToAdd
    in
        ( { model | itemToAdd = updatedItemToAdd }, Cmd.map DelegateToNewItemMsg cmd )


addItemCmd :
    Parameters newItemModel itemModel newItemMsg itemMsg factoryInput err
    -> Path
    -> Index
    -> factoryInput
    -> Cmd (Msg newItemMsg itemMsg itemModel factoryInput)
addItemCmd params path indexToAdd itemToAdd =
    let
        addItemToListCmd =
            params.binding.addItem path indexToAdd

        initItemWithItemToAddCmd =
            cmdOfMsg (DelegateToItemMsg indexToAdd (params.itemWidget.initMsg itemToAdd))
    in
        Cmd.batch [ addItemToListCmd, initItemWithItemToAddCmd ]


unselectPreviouslySelectedItems :
    Parameters newItemModel itemModel newItemMsg itemMsg factoryInput err
    -> Path
    -> List itemModel
    -> Index
    -> itemMsg
    -> ( List itemModel, Cmd (Msg newItemMsg itemMsg itemModel factoryInput) )
unselectPreviouslySelectedItems params path items exceptIndex msg =
    let
        unselectIfPreviouslySelected j itemModel =
            if params.itemWidget.isSelected itemModel && j /= exceptIndex then
                params.itemWidget.update params.itemWidget.unselectMsg itemModel (Index j :: path)
            else
                doNothing itemModel

        ( unselectedItems, unselectCmds ) =
            List.unzip (List.indexedMap unselectIfPreviouslySelected items)

        finalCmd =
            Cmd.batch <| List.indexedMap (\i -> Cmd.map (DelegateToItemMsg i)) unselectCmds
    in
        ( unselectedItems, finalCmd )


delegateUpdateToItem :
    Parameters newItemModel itemModel newItemMsg itemMsg factoryInput err
    -> Path
    -> List itemModel
    -> Index
    -> itemMsg
    -> ( List itemModel, Cmd (Msg newItemMsg itemMsg itemModel factoryInput) )
delegateUpdateToItem params path contents itemIndex itemMsg =
    case get contents itemIndex of
        Nothing ->
            doNothing contents

        Just subModel ->
            let
                ( updatedSubModel, cmd ) =
                    params.itemWidget.update itemMsg subModel (Index itemIndex :: path)
            in
                ( List.take itemIndex contents ++ [ updatedSubModel ] ++ List.drop (itemIndex + 1) contents
                , Cmd.map (DelegateToItemMsg itemIndex) cmd
                )



-- SUBSCRIPTIONS


subscriptions :
    Parameters newItemModel itemModel newItemMsg itemMsg factoryInput err
    -> Model newItemModel itemModel
    -> Path
    -> Sub (Msg newItemMsg itemMsg itemModel factoryInput)
subscriptions params model path =
    let
        bindingToMsg msgBuilder =
            Sub.map (Result.withDefault NoOp << Result.map msgBuilder)

        itemSub i itemModel =
            Sub.map (DelegateToItemMsg i) <| params.itemWidget.subscriptions itemModel (Index i :: path)
    in
        Sub.batch
            ([ bindingToMsg BackendAddedItem (params.binding.itemAdded path)
             , bindingToMsg BackendRemovedItem (params.binding.itemRemoved path)
             ]
                ++ List.indexedMap itemSub model.contents
            )



-- VIEW


view :
    Parameters newItemModel itemModel newItemMsg itemMsg factoryInput err
    -> Model newItemModel itemModel
    -> Html (Msg newItemMsg itemMsg itemModel factoryInput)
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
