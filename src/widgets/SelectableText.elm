module SelectableText exposing (..)

import Binding exposing (Binding, GenericBinding)
import Html exposing (Html, input, label, span, text)
import Html.Events exposing (onDoubleClick, onInput)
import Text
import Utils exposing (enterKey, onKeyUp)
import Widget exposing (BoundWidget, ISelectable, Unbound, Widget, cmdOfMsg, doNothing)


createWidget : GenericBinding Text.Msg String -> Widget Model Msg
createWidget binding id =
    let
        textWidget =
            Text.createWidget binding id
    in
        { initMsg = \m -> DelegateToTextMsg (Text.initMsg m)
        , initModel = initModel textWidget
        , update = update textWidget
        , view = view textWidget
        , subscriptions = subscriptions textWidget
        }


createSelectableWidget : GenericBinding Text.Msg String -> ISelectable Model Msg { widget : Widget Model Msg }
createSelectableWidget binding =
    { isSelected = .editMode
    , selectMsg = UISelect
    , unselectMsg = UIConfirm
    , widget = createWidget binding
    }



-- MODEL


type alias Model =
    { textModel : Text.Model
    , editMode : Bool
    }


initModel : BoundWidget Text.Model Text.Msg -> Model
initModel textWidget =
    { textModel = textWidget.initModel, editMode = False }



-- UPDATE


type Msg
    = DelegateToTextMsg Text.Msg
    | UISelect
    | UIConfirm


update : BoundWidget Text.Model Text.Msg -> Msg -> Model -> ( Model, Cmd Msg )
update textWidget msg model =
    case msg of
        DelegateToTextMsg textMsg ->
            let
                ( textModel, textCmd ) =
                    textWidget.update textMsg model.textModel

                cmd =
                    Cmd.map DelegateToTextMsg textCmd

                textUpdatedModel =
                    { model | textModel = textModel }
            in
                case textMsg of
                    Text.UICancel ->
                        ( { textUpdatedModel | editMode = False }, cmd )

                    Text.Init _ ->
                        ( { textUpdatedModel | editMode = False }, cmd )

                    Text.UIChange _ ->
                        {--We prevent Text to serialize the change: for a selectable text, this should only be done when confirmed. --}
                        doNothing textUpdatedModel

                    _ ->
                        ( textUpdatedModel, cmd )

        UISelect ->
            { model | editMode = True } |> doNothing

        UIConfirm ->
            let
                ( textModel, textCmd ) =
                    textWidget.update Text.ConfirmModel model.textModel
            in
                ( { model | textModel = textModel, editMode = False }, Cmd.map DelegateToTextMsg textCmd )



-- SUBSCRIPTIONS


subscriptions : BoundWidget Text.Model Text.Msg -> Model -> Sub Msg
subscriptions textWidget m =
    Sub.map DelegateToTextMsg (textWidget.subscriptions m.textModel)



-- VIEW


view : BoundWidget Text.Model Text.Msg -> Model -> Html Msg
view textWidget model =
    if model.editMode then
        span [ onKeyUp [ ( enterKey, UIConfirm ) ] ]
            [ Html.map DelegateToTextMsg <| textWidget.view model.textModel ]
    else
        label [ onDoubleClick UISelect ] [ text <| Text.getContent model.textModel ]
