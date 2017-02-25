module SelectableText exposing (..)

import Html exposing (Html, input, label, text)
import Html.Events exposing (onDoubleClick, onInput)
import Text
import Widget exposing (Binding, ISelectable, Path, Widget, cmdOfMsg, doNothing)


createWidget : Binding Text.Msg String err -> ISelectable Model Msg (Widget Model Msg String)
createWidget binding =
    let
        textWidget =
            Text.createWidget binding
    in
        { initMsg = DelegateToTextMsg << Text.Init
        , initModel = initModel textWidget
        , update = update textWidget
        , view = view textWidget
        , subscriptions = subscriptions textWidget
        , isSelected = .editMode
        , selectMsg = UISelect
        , unselectMsg = DelegateToTextMsg Text.UIConfirm
        }



-- MODEL


type alias Model =
    { textModel : Text.Model
    , editMode : Bool
    }


initModel : Widget Text.Model Text.Msg String -> Model
initModel textWidget =
    { textModel = textWidget.initModel, editMode = False }



-- UPDATE


type Msg
    = DelegateToTextMsg Text.Msg
    | UISelect


update : Widget Text.Model Text.Msg String -> Msg -> Model -> Path -> ( Model, Cmd Msg )
update textWidget msg model p =
    case msg of
        DelegateToTextMsg textMsg ->
            let
                ( textModel, textCmd ) =
                    textWidget.update textMsg model.textModel p

                cmd =
                    Cmd.map DelegateToTextMsg textCmd

                textUpdatedModel =
                    { model | textModel = textModel }
            in
                if textMsg == Text.UIConfirm || textMsg == Text.UICancel || Text.isInit textMsg then
                    ( { textUpdatedModel | editMode = False }, cmd )
                else
                    ( textUpdatedModel, cmd )

        UISelect ->
            { model | editMode = True } |> doNothing



-- SUBSCRIPTIONS


subscriptions : Widget Text.Model Text.Msg String -> Model -> Path -> Sub Msg
subscriptions textWidget m p =
    Sub.map DelegateToTextMsg <| textWidget.subscriptions m.textModel p



-- VIEW


view : Widget Text.Model Text.Msg String -> Model -> Html Msg
view textWidget model =
    if model.editMode then
        Html.map DelegateToTextMsg <| textWidget.view model.textModel
    else
        label [ onDoubleClick UISelect ] [ text model.textModel.uiContent ]
