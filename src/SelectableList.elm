module SelectableList exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Utils exposing (..)
import Widget exposing (SelectableWidget, DecisionWidget, ListBinding, Widget)


type alias ItemWidget model msg =
    Widget.SelectableWidget model msg


type alias NewItemWidget model msg =
    Widget.DecisionWidget model msg


createListWidget :
    ListBinding (Msg newItemMsg itemMsg itemModel) itemModel
    -> NewItemWidget itemModel newItemMsg
    -> ItemWidget itemModel itemMsg
    -> Widget (Model itemModel) (Msg newItemMsg itemMsg itemModel)
createListWidget binding newItemWidget itemWidget =
    { init = init newItemWidget
    , update = update binding newItemWidget itemWidget
    , subscriptions = subscriptions binding
    , view = view newItemWidget itemWidget
    }



-- MODEL


type alias Model itemModel =
    { itemToAdd : itemModel, contents : List itemModel }


init : NewItemWidget itemModel newItemMsg -> ( Model itemModel, Cmd (Msg newItemMsg itemMsg itemModel) )
init newItemWidget =
    let
        ( itemToAdd, cmdInit ) =
            newItemWidget.init
    in
        ( { itemToAdd = itemToAdd, contents = [] }, Cmd.map UINewItemMsg cmdInit )



-- UPDATE


type alias WidgetIndex =
    Int


type Msg newItemMsg itemMsg itemModel
    = UINewItemMsg newItemMsg
    | UIItemMsg WidgetIndex itemMsg
    | UIRemove WidgetIndex
    | ModelAddedItem WidgetIndex itemModel
    | ModelRemovedItem WidgetIndex


update :
    ListBinding (Msg newItemMsg itemMsg itemModel) itemModel
    -> NewItemWidget itemModel newItemMsg
    -> ItemWidget itemModel itemMsg
    -> Msg newItemMsg itemMsg itemModel
    -> Model itemModel
    -> ( Model itemModel, Cmd (Msg newItemMsg itemMsg itemModel) )
update binding newItemWidget itemWidget msg model =
    case msg of
        UINewItemMsg subMsg ->
            if subMsg == newItemWidget.confirmMsg then
                addItem binding newItemWidget model
            else
                let
                    ( updatedItemToAdd, cmd ) =
                        newItemWidget.update subMsg model.itemToAdd
                in
                    ( { model | itemToAdd = updatedItemToAdd }, Cmd.map UINewItemMsg cmd )

        UIItemMsg i subMsg ->
            propagateMsgToWidget itemWidget model i subMsg

        UIRemove i ->
            ( modelWithRemovedItem i model, binding.removeItem i )

        ModelAddedItem i m ->
            ( { model | contents = insert model.contents m i }, Cmd.none )

        ModelRemovedItem i ->
            ( modelWithRemovedItem i model, Cmd.none )


addItem :
    ListBinding (Msg newItemMsg itemMsg itemModel) itemModel
    -> NewItemWidget itemModel newItemMsg
    -> Model itemModel
    -> ( Model itemModel, Cmd (Msg newItemMsg itemMsg itemModel) )
addItem binding newItemWidget model =
    let
        ( newItemToAdd, newItemWidgetCmd ) =
            newItemWidget.init

        modelWithItemAdded =
            { itemToAdd = newItemToAdd, contents = model.itemToAdd :: model.contents }

        cmd =
            Cmd.batch [ binding.addItem 0 model.itemToAdd, Cmd.map UINewItemMsg newItemWidgetCmd ]
    in
        ( modelWithItemAdded, cmd )


propagateMsgToWidget : ItemWidget itemModel itemMsg -> Model itemModel -> WidgetIndex -> itemMsg -> ( Model itemModel, Cmd (Msg newItemMsg itemMsg itemModel) )
propagateMsgToWidget widget model i msg =
    let
        updatedWidgetList =
            List.indexedMap (updateWidget widget i msg) model.contents

        ( contentsWithWidgetUpdated, cmds ) =
            List.unzip updatedWidgetList
    in
        if msg /= widget.selectMsg then
            ( { model | contents = contentsWithWidgetUpdated }, Cmd.map (UIItemMsg i) <| Cmd.batch cmds )
        else
            let
                confirmWidget j m =
                    if j == i || not (widget.isSelected m) then
                        ( m, Cmd.none )
                    else
                        widget.update widget.unselectMsg m

                ( contents, widgetCmds ) =
                    List.unzip <| List.indexedMap confirmWidget contentsWithWidgetUpdated

                cmds =
                    List.indexedMap (Cmd.map << UIItemMsg) widgetCmds
            in
                ( { model | contents = contents }, Cmd.batch cmds )


modelWithRemovedItem : WidgetIndex -> Model itemModel -> Model itemModel
modelWithRemovedItem i model =
    { model | contents = List.take i model.contents ++ List.drop (i + 1) model.contents }


updateWidget :
    ItemWidget itemModel itemMsg
    -> WidgetIndex
    -> itemMsg
    -> WidgetIndex
    -> itemModel
    -> ( itemModel, Cmd itemMsg )
updateWidget widget refIndex msg candidateIndex model =
    if candidateIndex == refIndex then
        widget.update msg model
    else
        ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions :
    ListBinding (Msg newItemMsg itemMsg itemModel) itemModel
    -> Model itemModel
    -> Sub (Msg newItemMsg itemMsg itemModel)
subscriptions binding _ =
    Sub.batch
        [ Sub.map (\( i, v ) -> ModelAddedItem i v) binding.itemAdded
        , Sub.map ModelRemovedItem binding.itemRemoved
        ]



-- VIEW


view :
    NewItemWidget itemModel newItemMsg
    -> ItemWidget itemModel itemMsg
    -> Model itemModel
    -> Html (Msg newItemMsg itemMsg itemModel)
view newItemWidget widget model =
    ul [] <|
        li [] [ Html.map UINewItemMsg <| newItemWidget.view model.itemToAdd ]
            :: List.indexedMap (viewWidget widget) model.contents


viewWidget : ItemWidget itemModel itemMsg -> WidgetIndex -> itemModel -> Html (Msg newItemMsg itemMsg itemModel)
viewWidget widget i m =
    li []
        [ span []
            [ Html.map (UIItemMsg i) <| widget.view m
            , button [ onClick <| UIRemove i ] [ text "-" ]
            ]
        ]
