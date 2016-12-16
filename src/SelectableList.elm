module SelectableList exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Utils exposing (..)


type alias Selectable model msg base =
    { base
        | isSelected : model -> Bool
        , selectMsg : msg
        , unselectMsg : msg
        , confirmMsg :
            msg
            {--TODO separate item widget from addition widget --}
    }


type alias ItemWidget model msg =
    Selectable model msg (Widget model msg)


createListWidget : ItemWidget itemModel itemMsg -> Widget (Model itemModel) (Msg itemMsg)
createListWidget widget =
    { init = init widget, update = update widget, subscriptions = emptySubscription, view = view widget }



-- MODEL


type alias Model itemModel =
    { itemToAdd : itemModel, contents : List itemModel }


init : ItemWidget itemModel itemMsg -> ( Model itemModel, Cmd (Msg itemMsg) )
init widget =
    let
        ( itemToAdd, cmdInit ) =
            widget.init
    in
        ( { itemToAdd = itemToAdd, contents = [] }, Cmd.map (WidgetMsg 0) cmdInit )



-- UPDATE


type alias WidgetIndex =
    -- 0 is for the item to add, items in the list are counted starting from 1.
    Int


type Msg itemMsg
    = Add
    | WidgetMsg WidgetIndex itemMsg
    | Remove WidgetIndex


update : ItemWidget itemModel itemMsg -> Msg itemMsg -> Model itemModel -> ( Model itemModel, Cmd (Msg itemMsg) )
update widget msg model =
    case msg of
        Add ->
            addItem widget model

        WidgetMsg i msg ->
            if i == 0 then
                let
                    ( updatedItemToAdd, cmd ) =
                        widget.update msg model.itemToAdd
                in
                    if msg == widget.confirmMsg then
                        let
                            ( itemAdded, cmdAddition ) =
                                addItem widget { model | itemToAdd = updatedItemToAdd }
                        in
                            ( itemAdded, Cmd.batch [ Cmd.map (WidgetMsg 1) cmd, cmdAddition ] )
                    else if msg /= widget.unselectMsg then
                        ( { model | itemToAdd = updatedItemToAdd }, Cmd.map (WidgetMsg 0) cmd )
                    else
                        ( model, Cmd.none )
            else
                propagateMsgToWidget widget model (i - 1) msg

        Remove i ->
            removeItem (i - 1) model


addItem : ItemWidget itemModel itemMsg -> Model itemModel -> ( Model itemModel, Cmd (Msg itemMsg) )
addItem widget model =
    let
        ( initModel, initCmd ) =
            widget.init

        ( initModelSelected, selectCmd ) =
            widget.update widget.selectMsg initModel

        ( itemAddedUnselected, unselectCmd ) =
            widget.update widget.unselectMsg model.itemToAdd

        modelWithItemAdded =
            { itemToAdd = initModelSelected, contents = model.itemToAdd :: model.contents }
    in
        ( modelWithItemAdded, Cmd.map (WidgetMsg 1) <| Cmd.batch [ initCmd, selectCmd ] )


propagateMsgToWidget : ItemWidget itemModel itemMsg -> Model itemModel -> WidgetIndex -> itemMsg -> ( Model itemModel, Cmd (Msg itemMsg) )
propagateMsgToWidget widget model i msg =
    let
        updatedWidgetList =
            List.indexedMap (updateWidget widget i msg) model.contents

        ( contentsWithWidgetUpdated, cmds ) =
            List.unzip updatedWidgetList
    in
        if msg /= widget.selectMsg then
            ( { model | contents = contentsWithWidgetUpdated }, Cmd.map (WidgetMsg i) <| Cmd.batch cmds )
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
                    List.indexedMap (\i -> Cmd.map (WidgetMsg i)) widgetCmds
            in
                ( { model | contents = contents }, Cmd.batch cmds )


removeItem : WidgetIndex -> Model itemModel -> ( Model itemModel, Cmd (Msg itemMsg) )
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


view : ItemWidget itemModel itemMsg -> Model itemModel -> Html (Msg itemMsg)
view widget model =
    ul [] <|
        li [] [ Html.map (WidgetMsg 0) <| widget.view model.itemToAdd ]
            :: List.indexedMap (\i -> viewWidget widget (i + 1)) model.contents


viewWidget : ItemWidget itemModel itemMsg -> WidgetIndex -> itemModel -> Html (Msg itemMsg)
viewWidget widget i m =
    li []
        [ span []
            [ Html.map (WidgetMsg i) <| widget.view m
            , button [ onClick <| Remove i ] [ text "-" ]
            ]
        ]
