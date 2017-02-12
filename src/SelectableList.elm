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
    = UINewItemMsg newItemMsg
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
            UINewItemMsg subMsg ->
                if subMsg == newItemWidget.confirmMsg then
                    addItem params model p
                else
                    let
                        ( updatedItemToAdd, cmd ) =
                            newItemWidget.update subMsg model.itemToAdd
                    in
                        ( { model | itemToAdd = updatedItemToAdd }, Cmd.map UINewItemMsg cmd )

            ItemMsg i subMsg ->
                propagateMsgToWidget itemWidget model i subMsg p

            UIRemove i ->
                ( model, binding.removeItem p i )

            ModelAddedItem i ->
                ( { model | contents = insert model.contents itemWidget.initModel i }, binding.askItemContent p i )

            ModelRemovedItem i ->
                doNothing (modelWithRemovedItem i model)

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


propagateMsgToWidget :
    ItemWidget itemModel itemMsg factoryInput
    -> Model newItemModel itemModel
    -> Index
    -> itemMsg
    -> Widget.Path
    -> ( Model newItemModel itemModel, Cmd (Msg newItemMsg itemMsg itemModel factoryInput) )
propagateMsgToWidget widget model i msg p =
    let
        updatedWidgetList =
            List.indexedMap (updateWidget widget i msg p) model.contents

        ( contentsWithWidgetUpdated, updateCmds ) =
            List.unzip updatedWidgetList

        updateCmd =
            Cmd.batch <| List.map (Cmd.map (ItemMsg i)) updateCmds
    in
        if msg /= widget.selectMsg then
            ( { model | contents = contentsWithWidgetUpdated }, updateCmd )
        else
            let
                unselectPreviouslySelected j itemModel =
                    if widget.isSelected itemModel && j /= i then
                        widget.update widget.unselectMsg itemModel (Index j :: p)
                    else
                        doNothing itemModel

                ( contents, unselectCmds ) =
                    List.unzip <| List.indexedMap unselectPreviouslySelected contentsWithWidgetUpdated

                finalCmd =
                    Cmd.batch <| updateCmd :: List.indexedMap (\i -> Cmd.map (ItemMsg i)) unselectCmds
            in
                ( { model | contents = contents }, finalCmd )


modelWithRemovedItem : Index -> Model newItemModel itemModel -> Model newItemModel itemModel
modelWithRemovedItem i model =
    { model | contents = List.take i model.contents ++ List.drop (i + 1) model.contents }


updateWidget :
    ItemWidget itemModel itemMsg factoryInput
    -> Index
    -> itemMsg
    -> Widget.Path
    -> Index
    -> itemModel
    -> ( itemModel, Cmd itemMsg )
updateWidget widget refIndex msg p candidateIndex model =
    if candidateIndex == refIndex then
        widget.update msg model (Index candidateIndex :: p)
    else
        doNothing model



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
    in
        ul [] <|
            li [] [ Html.map UINewItemMsg <| newItemWidget.view model.itemToAdd ]
                :: List.indexedMap (viewWidget itemWidget) model.contents


viewWidget :
    ItemWidget itemModel itemMsg factoryInput
    -> Index
    -> itemModel
    -> Html (Msg newItemMsg itemMsg itemModel factoryInput)
viewWidget widget i m =
    li []
        [ span []
            [ Html.map (ItemMsg i) <| widget.view m
            , button [ onClick <| UIRemove i ] [ text "-" ]
            ]
        ]
