module SelectableList exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Utils exposing (..)
import Widget exposing (Path, IDecision, ISelectable, Index, ListBinding, UnboundWidget, Widget, cmdOfMsg, doNothing)


type alias ItemWidget model msg factoryInput =
    ISelectable model msg (Widget model msg factoryInput)


type alias NewItemWidget model msg factoryInput =
    IDecision msg (UnboundWidget model msg factoryInput)


type alias Converter fromModel toModelFactoryInput =
    fromModel -> toModelFactoryInput


type alias Parameters newItemModel itemModel newItemMsg itemMsg factoryInput err =
    ( ListBinding (Msg newItemMsg itemMsg itemModel factoryInput) err, NewItemWidget newItemModel newItemMsg factoryInput, ItemWidget itemModel itemMsg factoryInput, Converter newItemModel factoryInput )


createListWidget :
    Parameters newItemModel itemModel newItemMsg itemMsg factoryInput err
    -> Widget (Model newItemModel itemModel) (Msg newItemMsg itemMsg itemModel factoryInput) (List factoryInput)
createListWidget params =
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
emptyModel ( binding, newItemWidget, itemWidget, converter ) =
    { itemToAdd = newItemWidget.initModel, contents = [] }



-- UPDATE


type Msg newItemMsg itemMsg itemModel factoryInput
    = DelegateToNewItemMsg newItemMsg
    | DelegateToItemMsg Index itemMsg
    | Remove Index
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
    let
        ( binding, newItemWidget, itemWidget, converter ) =
            params
    in
        case msg of
            DelegateToNewItemMsg subMsg ->
                if subMsg == newItemWidget.confirmMsg then
                    ( { model | itemToAdd = newItemWidget.initModel }
                    , addItemCmd params path 0 (converter model.itemToAdd)
                    )
                else
                    delegateUpdateToItemToAdd params model subMsg

            DelegateToItemMsg i subMsg ->
                let
                    ( updatedItems, updateCmd ) =
                        delegateUpdateToItem params path model.contents i subMsg

                    ( unselectedUpdatedItems, unselectCmds ) =
                        if subMsg == itemWidget.selectMsg then
                            unselectPreviouslySelectedItems params path updatedItems i subMsg
                        else
                            ( updatedItems, Cmd.none )
                in
                    ( { model | contents = unselectedUpdatedItems }, Cmd.batch [ updateCmd, unselectCmds ] )

            Remove i ->
                ( model, binding.removeItem path i )

            BackendAddedItem i ->
                ( { model | contents = insert model.contents itemWidget.initModel i }, binding.askItemContent path i )

            BackendRemovedItem i ->
                doNothing ({ model | contents = List.take i model.contents ++ List.drop (i + 1) model.contents })

            Init l ->
                ( emptyModel params, Cmd.batch (List.indexedMap (\i fi -> addItemCmd params path i fi) l) )

            NoOp ->
                doNothing model


delegateUpdateToItemToAdd :
    Parameters newItemModel itemModel newItemMsg itemMsg factoryInput err
    -> Model newItemModel itemModel
    -> newItemMsg
    -> ( Model newItemModel itemModel, Cmd (Msg newItemMsg itemMsg itemModel factoryInput) )
delegateUpdateToItemToAdd ( _, newItemWidget, _, _ ) model subMsg =
    let
        ( updatedItemToAdd, cmd ) =
            newItemWidget.update subMsg model.itemToAdd
    in
        ( { model | itemToAdd = updatedItemToAdd }, Cmd.map DelegateToNewItemMsg cmd )


addItemCmd :
    Parameters newItemModel itemModel newItemMsg itemMsg factoryInput err
    -> Path
    -> Index
    -> factoryInput
    -> Cmd (Msg newItemMsg itemMsg itemModel factoryInput)
addItemCmd ( binding, _, itemWidget, _ ) path indexToAdd itemToAdd =
    let
        addItemToListCmd =
            binding.addItem path indexToAdd

        initItemWithItemToAddCmd =
            cmdOfMsg (DelegateToItemMsg indexToAdd (itemWidget.initMsg itemToAdd))
    in
        Cmd.batch [ addItemToListCmd, initItemWithItemToAddCmd ]


unselectPreviouslySelectedItems :
    Parameters newItemModel itemModel newItemMsg itemMsg factoryInput err
    -> Path
    -> List itemModel
    -> Index
    -> itemMsg
    -> ( List itemModel, Cmd (Msg newItemMsg itemMsg itemModel factoryInput) )
unselectPreviouslySelectedItems ( _, _, itemWidget, _ ) path items exceptIndex msg =
    let
        unselectIfPreviouslySelected j itemModel =
            if itemWidget.isSelected itemModel && j /= exceptIndex then
                itemWidget.update itemWidget.unselectMsg itemModel (Index j :: path)
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
delegateUpdateToItem ( _, _, itemWidget, _ ) path contents itemIndex itemMsg =
    case get contents itemIndex of
        Nothing ->
            doNothing contents

        Just subModel ->
            let
                ( updatedSubModel, cmd ) =
                    itemWidget.update itemMsg subModel (Index itemIndex :: path)
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
subscriptions ( binding, _, itemWidget, _ ) model path =
    let
        bindingToMsg msgBuilder =
            Sub.map (Result.withDefault NoOp << Result.map msgBuilder)

        itemSub i itemModel =
            Sub.map (DelegateToItemMsg i) <| itemWidget.subscriptions itemModel (Index i :: path)
    in
        Sub.batch
            ([ bindingToMsg BackendAddedItem (binding.itemAdded path)
             , bindingToMsg BackendRemovedItem (binding.itemRemoved path)
             ]
                ++ List.indexedMap itemSub model.contents
            )



-- VIEW


view :
    Parameters newItemModel itemModel newItemMsg itemMsg factoryInput err
    -> Model newItemModel itemModel
    -> Html (Msg newItemMsg itemMsg itemModel factoryInput)
view ( _, newItemWidget, itemWidget, _ ) model =
    let
        delegateViewToItem i m =
            li []
                [ span []
                    [ Html.map (DelegateToItemMsg i) <| itemWidget.view m
                    , button [ onClick <| Remove i ] [ text "-" ]
                    ]
                ]
    in
        ul [] <|
            li [] [ Html.map DelegateToNewItemMsg <| newItemWidget.view model.itemToAdd ]
                :: List.indexedMap delegateViewToItem model.contents
