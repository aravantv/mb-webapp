module SelectableText exposing (..)

import Binding exposing (Binding, BindingResult, BindingSubInfo, BindingUpInfo(..), BoundWidget, doNothing)
import Html exposing (Html, input, label, span, text)
import Html.Events exposing (onDoubleClick, onInput)
import Text
import Utils exposing (enterKey, onKeyUp)
import Widget exposing (ISelectable, Widget, cmdOf, cmdOfMsg, modelOf)


widget : BoundWidget Model Msg String
widget =
    { init = init Text.widget
    , update = update Text.widget
    , view = view Text.widget
    , subscriptions = subscriptions Text.widget
    }


selectableWidget : ISelectable Model Msg { widget : BoundWidget Model Msg String }
selectableWidget =
    { isSelected = .editMode
    , selectMsg = UISelect
    , unselectMsg = UIConfirm
    , widget = widget
    }



-- MODEL


type alias Model =
    { textModel : Text.Model
    , editMode : Bool
    }


init : BoundWidget Text.Model Text.Msg String -> ( Model, Cmd Msg )
init textWidget =
    ( { textModel = modelOf textWidget.init, editMode = False }
    , Cmd.map DelegateToTextMsg (cmdOf textWidget.init)
    )



-- UPDATE


type Msg
    = DelegateToTextMsg Text.Msg
    | UISelect
    | UIConfirm


update : BoundWidget Text.Model Text.Msg String -> Msg -> Model -> ( Model, Cmd Msg, BindingUpInfo String )
update textWidget msg model =
    case msg of
        DelegateToTextMsg textMsg ->
            let
                ( textModel, textCmd, setInfo ) =
                    textWidget.update textMsg model.textModel

                cmd =
                    Cmd.map DelegateToTextMsg textCmd

                textUpdatedModel =
                    { model | textModel = textModel }
            in
                case textMsg of
                    Text.UICancel ->
                        ( { textUpdatedModel | editMode = False }, cmd, DoNothing )

                    Text.UIChange _ ->
                        {--We prevent Text to serialize the change: for a selectable text, this should only be done when confirmed. --}
                        doNothing textUpdatedModel

                    _ ->
                        ( textUpdatedModel, cmd, setInfo )

        UISelect ->
            { model | editMode = True } |> doNothing

        UIConfirm ->
            let
                ( textModel, textCmd, upInfo ) =
                    textWidget.update Text.ConfirmModel model.textModel
            in
                ( { model | textModel = textModel, editMode = False }, Cmd.map DelegateToTextMsg textCmd, upInfo )



-- SUBSCRIPTIONS


subscriptions : BoundWidget Text.Model Text.Msg String -> Model -> ( Sub Msg, BindingSubInfo String Msg )
subscriptions textWidget m =
    let
        ( sub, subInfo ) =
            textWidget.subscriptions m.textModel
    in
        ( Sub.map DelegateToTextMsg sub, DelegateToTextMsg << subInfo )



-- VIEW


view : BoundWidget Text.Model Text.Msg String -> Model -> Html Msg
view textWidget model =
    if model.editMode then
        span [ onKeyUp [ ( enterKey, UIConfirm ) ] ]
            [ Html.map DelegateToTextMsg <| textWidget.view model.textModel ]
    else
        label [ onDoubleClick UISelect ] [ text <| Text.getContent model.textModel ]
