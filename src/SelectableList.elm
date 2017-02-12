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
    = NewItemMsg newItemMsg
    | ItemMsg Index itemMsg
    | UIRemove Index
    | ModelAddedItem Index
    | ModelRemovedItem Index
    | NoOp
    | Init (List factoryInput)


update :
    Parameters newItemModel itemModel newItemMsg itemMsg factoryInput err
    -> Msg newItemMsg itemMsg itemModel factoryInput
    -> Model newItemModel itemModel
    -> Path
    -> ( Model newItemModel itemModel, Cmd (Msg newItemMsg itemMsg itemModel factoryInput) )
update params msg model p =
    let
        ( binding, newItemWidget, itemWidget, converter ) =
            params
    in
        case msg of
            NewItemMsg subMsg ->
                if subMsg == newItemWidget.confirmMsg then
                    addItem params model p
                else
                    let
                        ( updatedItemToAdd, cmd ) =
                            newItemWidget.update subMsg model.itemToAdd
                    in
                        ( { model | itemToAdd = updatedItemToAdd }, Cmd.map NewItemMsg cmd )

            ItemMsg i subMsg ->
                let
                    ( updatedItems, updateCmd ) =
                        updateSpecificItemOnly itemWidget model.contents p i subMsg
                in
                    if subMsg == itemWidget.selectMsg then
                        let
                            ( unselectedContents, unselectCmds ) =
                                unselectPreviouslySelectedItems itemWidget updatedItems i subMsg p
                        in
                            ( { model | contents = unselectedContents }, Cmd.batch [ updateCmd, unselectCmds ] )
                    else
                        ( { model | contents = updatedItems }, updateCmd )

            UIRemove i ->
                ( model, binding.removeItem p i )

            ModelAddedItem i ->
                ( { model | contents = insert model.contents itemWidget.initModel i }, binding.askItemContent p i )

            ModelRemovedItem i ->
                doNothing ({ model | contents = List.take i model.contents ++ List.drop (i + 1) model.contents })

            Init l ->
                let
                    addItemCmd i fi =
                        Cmd.batch [ binding.addItem p i, cmdOfMsg (ItemMsg i (itemWidget.initMsg fi)) ]
                in
                    ( emptyModel params, Cmd.batch (List.indexedMap addItemCmd l) )

            NoOp ->
                doNothing model


addItem :
    Parameters newItemModel itemModel newItemMsg itemMsg factoryInput err
    -> Model newItemModel itemModel
    -> Path
    -> ( Model newItemModel itemModel, Cmd (Msg newItemMsg itemMsg itemModel factoryInput) )
addItem ( binding, newItemWidget, itemWidget, converter ) model p =
    let
        cmd =
            Cmd.batch
                [ binding.addItem p 0
                , cmdOfMsg <| ItemMsg 0 <| itemWidget.initMsg <| converter model.itemToAdd
                ]

        modelWithItemAdded =
            { itemToAdd = newItemWidget.initModel, contents = model.contents }
    in
        ( modelWithItemAdded, cmd )


unselectPreviouslySelectedItems :
    ItemWidget itemModel itemMsg factoryInput
    -> List itemModel
    -> Index
    -> itemMsg
    -> Widget.Path
    -> ( List itemModel, Cmd (Msg newItemMsg itemMsg itemModel factoryInput) )
unselectPreviouslySelectedItems itemWidget items i msg path =
    let
        unselectIfPreviouslySelected j itemModel =
            if itemWidget.isSelected itemModel && j /= i then
                itemWidget.update itemWidget.unselectMsg itemModel (Index j :: path)
            else
                doNothing itemModel

        ( unselectedItems, unselectCmds ) =
            List.unzip (List.indexedMap unselectIfPreviouslySelected items)

        finalCmd =
            Cmd.batch <| List.indexedMap (\i -> Cmd.map (ItemMsg i)) unselectCmds
    in
        ( unselectedItems, finalCmd )


updateSpecificItemOnly :
    ItemWidget itemModel itemMsg factoryInput
    -> List itemModel
    -> Widget.Path
    -> Index
    -> itemMsg
    -> ( List itemModel, Cmd (Msg newItemMsg itemMsg itemModel factoryInput) )
updateSpecificItemOnly itemWidget contents path itemIndex itemMsg =
    let
        subUpdate subModel =
            itemWidget.update itemMsg subModel (Index itemIndex :: path)

        maybeItemUpdated =
            Maybe.map subUpdate (get contents itemIndex)
    in
        case maybeItemUpdated of
            Nothing ->
                doNothing contents

            Just ( subModel, cmd ) ->
                ( List.take itemIndex contents ++ [ subModel ] ++ List.drop (itemIndex + 1) contents
                , Cmd.map (ItemMsg itemIndex) cmd
                )



-- SUBSCRIPTIONS


subscriptions :
    Parameters newItemModel itemModel newItemMsg itemMsg factoryInput err
    -> Model newItemModel itemModel
    -> Widget.Path
    -> Sub (Msg newItemMsg itemMsg itemModel factoryInput)
subscriptions params model p =
    let
        ( binding, _, itemWidget, _ ) =
            params

        bindingToMsg msgBuilder =
            Sub.map (Result.withDefault NoOp << Result.map msgBuilder)

        itemSub i itemModel =
            Sub.map (ItemMsg i) <| itemWidget.subscriptions itemModel (Index i :: p)
    in
        Sub.batch
            ([ bindingToMsg ModelAddedItem (binding.itemAdded p)
             , bindingToMsg ModelRemovedItem (binding.itemRemoved p)
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
        ( binding, newItemWidget, itemWidget, converter ) =
            params

        viewWidget widget i m =
            li []
                [ span []
                    [ Html.map (ItemMsg i) <| widget.view m
                    , button [ onClick <| UIRemove i ] [ text "-" ]
                    ]
                ]
    in
        ul [] <|
            li [] [ Html.map NewItemMsg <| newItemWidget.view model.itemToAdd ]
                :: List.indexedMap (viewWidget itemWidget) model.contents
