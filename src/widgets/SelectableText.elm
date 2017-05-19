module SelectableText exposing (..)

import Binding exposing (Binding, BindingSubInfo, BindingUpInfo(..), BoundWidgetWithBinding, WidgetWithBinding, doNothing)
import Html exposing (Html, input, label, span, text)
import Html.Events exposing (onDoubleClick, onInput)
import Text
import Utils exposing (enterKey, onKeyUp)
import Widget exposing (BoundWidget, ISelectable, Unbound, Widget, cmdOfMsg)


widget : WidgetWithBinding Model Msg String
widget id =
    let
        textWidget =
            Text.widget id
    in
        { initMsg = \m -> DelegateToTextMsg (Text.initMsg m)
        , initModel = initModel textWidget
        , update = update textWidget
        , view = view textWidget
        , subscriptions = subscriptions textWidget
        }


selectableWidget : ISelectable Model Msg { widget : WidgetWithBinding Model Msg String }
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


initModel : BoundWidgetWithBinding Text.Model Text.Msg String -> Model
initModel textWidget =
    { textModel = textWidget.initModel, editMode = False }



-- UPDATE


type Msg
    = DelegateToTextMsg Text.Msg
    | UISelect
    | UIConfirm


update : BoundWidgetWithBinding Text.Model Text.Msg String -> Msg -> Model -> ( Model, Cmd Msg, BindingUpInfo String )
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

                    Text.Init _ ->
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


subscriptions : BoundWidgetWithBinding Text.Model Text.Msg String -> Model -> ( Sub Msg, BindingSubInfo String Msg )
subscriptions textWidget m =
    let
        ( sub, subInfo ) =
            textWidget.subscriptions m.textModel
    in
        ( Sub.map DelegateToTextMsg sub, DelegateToTextMsg << subInfo )



-- VIEW


view : BoundWidgetWithBinding Text.Model Text.Msg String -> Model -> Html Msg
view textWidget model =
    if model.editMode then
        span [ onKeyUp [ ( enterKey, UIConfirm ) ] ]
            [ Html.map DelegateToTextMsg <| textWidget.view model.textModel ]
    else
        label [ onDoubleClick UISelect ] [ text <| Text.getContent model.textModel ]
