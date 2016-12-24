module SelectableList exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Utils exposing (..)
import Widget


type alias ItemWidget datamodel model msg =
    Widget.Selectable model msg (Widget datamodel model msg)


type alias NewItemWidget datamodel model msg =
    Widget.Decision msg (Widget datamodel model msg)


type alias ListDataBinding dataModel itemModel =
    { toList : dataModel -> List itemModel
    , fromList : List itemModel -> dataModel
    }


createListWidget :
    ListDataBinding dataModel itemModel
    -> NewItemWidget dataModel itemModel newItemMsg
    -> ItemWidget dataModel itemModel itemMsg
    -> Widget dataModel (Model itemModel) (Msg newItemMsg itemMsg)
createListWidget binding newItemWidget itemWidget =
    { init = init newItemWidget
    , update = update newItemWidget itemWidget
    , subscriptions = emptySubscription
    , view = view newItemWidget itemWidget
    }



-- MODEL


type alias Model itemModel =
    { itemToAdd : itemModel, contents : List itemModel }


init : NewItemWidget itemModel newItemMsg -> ( Model itemModel, Cmd (Msg newItemMsg itemMsg) )
init newItemWidget =
    let
        ( itemToAdd, cmdInit ) =
            newItemWidget.init
    in
        ( { itemToAdd = itemToAdd, contents = [] }, Cmd.map NewItemMsg cmdInit )



-- UPDATE


type alias WidgetIndex =
    Int


type Msg newItemMsg itemMsg
    = NewItemMsg newItemMsg
    | ItemMsg WidgetIndex itemMsg
    | Remove WidgetIndex


update :
    NewItemWidget itemModel newItemMsg
    -> ItemWidget itemModel itemMsg
    -> Msg newItemMsg itemMsg
    -> Model itemModel
    -> ( Model itemModel, Cmd (Msg newItemMsg itemMsg) )
update newItemWidget itemWidget msg model =
    case msg of
        NewItemMsg subMsg ->
            if subMsg == newItemWidget.confirmMsg then
                addItem newItemWidget model
            else
                let
                    ( updatedItemToAdd, cmd ) =
                        newItemWidget.update subMsg model.itemToAdd
                in
                    ( { model | itemToAdd = updatedItemToAdd }, Cmd.map NewItemMsg cmd )

        ItemMsg i subMsg ->
            propagateMsgToWidget itemWidget model i subMsg

        Remove i ->
            removeItem i model


addItem : NewItemWidget itemModel newItemMsg -> Model itemModel -> ( Model itemModel, Cmd (Msg newItemMsg itemMsg) )
addItem newItemWidget model =
    let
        ( newItemToAdd, newItemWidgetCmd ) =
            newItemWidget.init

        modelWithItemAdded =
            { itemToAdd = newItemToAdd, contents = model.itemToAdd :: model.contents }
    in
        ( modelWithItemAdded, Cmd.map NewItemMsg newItemWidgetCmd )


propagateMsgToWidget : ItemWidget itemModel itemMsg -> Model itemModel -> WidgetIndex -> itemMsg -> ( Model itemModel, Cmd (Msg newItemMsg itemMsg) )
propagateMsgToWidget widget model i msg =
    let
        updatedWidgetList =
            List.indexedMap (updateWidget widget i msg) model.contents

        ( contentsWithWidgetUpdated, cmds ) =
            List.unzip updatedWidgetList
    in
        if msg /= widget.selectMsg then
            ( { model | contents = contentsWithWidgetUpdated }, Cmd.map (ItemMsg i) <| Cmd.batch cmds )
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
                    List.indexedMap (Cmd.map << ItemMsg) widgetCmds
            in
                ( { model | contents = contents }, Cmd.batch cmds )


removeItem : WidgetIndex -> Model itemModel -> ( Model itemModel, Cmd (Msg newItemMsg itemMsg) )
removeItem i model =
    let
        before_i =
            List.take i model.contents

        after_i =
            List.drop (i + 1) model.contents
    in
        ( { model | contents = before_i ++ after_i }, Cmd.none )


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



-- VIEW


view :
    NewItemWidget itemModel newItemMsg
    -> ItemWidget itemModel itemMsg
    -> Model itemModel
    -> Html (Msg newItemMsg itemMsg)
view newItemWidget widget model =
    ul [] <|
        li [] [ Html.map NewItemMsg <| newItemWidget.view model.itemToAdd ]
            :: List.indexedMap (viewWidget widget) model.contents


viewWidget : ItemWidget itemModel itemMsg -> WidgetIndex -> itemModel -> Html (Msg newItemMsg itemMsg)
viewWidget widget i m =
    li []
        [ span []
            [ Html.map (ItemMsg i) <| widget.view m
            , button [ onClick <| Remove i ] [ text "-" ]
            ]
        ]
