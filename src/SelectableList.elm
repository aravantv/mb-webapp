module SelectableList exposing (..)

import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events exposing (onInput, onClick)
import Utils exposing (..)


type alias Selectable model msg base =
    { base
        | isSelected : model -> Bool
        , selectMsg : msg
        , unselectMsg : msg
        , change :
            String
            -> msg
            {--TODO remove change!--}
    }


type alias ItemWidget model msg =
    Selectable model msg (Widget model msg)


createListWidget : ItemWidget itemModel itemMsg -> Widget (Model itemModel) (Msg itemMsg)
createListWidget widget =
    { init = init, update = update widget, subscriptions = emptySubscription, view = view widget }



-- MODEL


type alias Model itemModel =
    { uiToAdd : String, contents : List itemModel }


init : ( Model itemModel, Cmd (Msg itemMsg) )
init =
    ( { uiToAdd = "", contents = [] }, Cmd.none )



-- UPDATE


type alias WidgetIndex =
    Int


type Msg itemMsg
    = Add
    | ChangeToAdd String
    | WidgetMsg WidgetIndex itemMsg
    | Remove WidgetIndex


update : ItemWidget itemModel itemMsg -> Msg itemMsg -> Model itemModel -> ( Model itemModel, Cmd (Msg itemMsg) )
update widget msg model =
    case msg of
        Add ->
            let
                ( initModel, cmd1 ) =
                    widget.init

                modelWithEmptyItemAdded =
                    { uiToAdd = "", contents = initModel :: model.contents }

                ( modelWithItemChanged, cmd2 ) =
                    update widget (WidgetMsg 0 <| widget.change model.uiToAdd) modelWithEmptyItemAdded

                ( modelWithItemConfirmed, cmd3 ) =
                    update widget (WidgetMsg 0 widget.unselectMsg) modelWithItemChanged

                cmd =
                    Cmd.batch ([ Cmd.map (WidgetMsg 0) cmd1, cmd2, cmd3 ])
            in
                ( modelWithItemConfirmed, cmd )

        ChangeToAdd s ->
            ( { model | uiToAdd = s }, Cmd.none )

        WidgetMsg i msg ->
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

        Remove i ->
            removeItem i model


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
        (li [] [ viewToAddField model ])
            :: List.indexedMap (viewWidget widget) model.contents


viewToAddField : Model itemModel -> Html (Msg itemMsg)
viewToAddField model =
    input
        [ onInput ChangeToAdd
        , onKeyUp [ ( enterKey, Add ) ]
        , Attributes.value model.uiToAdd
        ]
        []


viewWidget : ItemWidget itemModel itemMsg -> WidgetIndex -> itemModel -> Html (Msg itemMsg)
viewWidget widget i m =
    li []
        [ span []
            [ Html.map (WidgetMsg i) <| widget.view m
            , button [ onClick <| Remove i ] [ text "-" ]
            ]
        ]
