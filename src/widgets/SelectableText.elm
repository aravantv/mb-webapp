module SelectableText exposing (..)

import Binding exposing (Binding)
import Html exposing (Html, input, label, span, text)
import Html.Events exposing (onDoubleClick, onInput)
import MetaModel exposing (ModelElementIdentifier)
import Text
import Utils exposing (enterKey, onKeyUp)
import Widget exposing (ISelectable, Widget, cmdOfMsg, doNothing)


createSelectableWidget : Binding Text.Msg String -> ISelectable Model Msg (Widget Model Msg)
createSelectableWidget binding =
    let
        textWidget =
            Text.createWidget binding
    in
        { initMsg = \m -> DelegateToTextMsg (Text.initMsg m)
        , initModel = initModel textWidget
        , update = update textWidget
        , view = view textWidget
        , subscriptions = subscriptions textWidget
        , isSelected = .editMode
        , selectMsg = UISelect
        , unselectMsg = UIConfirm
        }


createWidget : Binding Text.Msg String -> Widget Model Msg
createWidget binding =
    let
        textWidget =
            Text.createWidget binding
    in
        { initMsg = \m -> DelegateToTextMsg (Text.initMsg m)
        , initModel = initModel textWidget
        , update = update textWidget
        , view = view textWidget
        , subscriptions = subscriptions textWidget
        }



-- MODEL


type alias Model =
    { textModel : Text.Model
    , editMode : Bool
    }


initModel : Widget Text.Model Text.Msg -> Model
initModel textWidget =
    { textModel = textWidget.initModel, editMode = False }



-- UPDATE


type Msg
    = DelegateToTextMsg Text.Msg
    | UISelect
    | UIConfirm


update : Widget Text.Model Text.Msg -> Msg -> Model -> ModelElementIdentifier -> ( Model, Cmd Msg )
update textWidget msg model id =
    case msg of
        DelegateToTextMsg textMsg ->
            let
                ( textModel, textCmd ) =
                    textWidget.update textMsg model.textModel id

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
                    textWidget.update Text.ConfirmModel model.textModel id
            in
                ( { model | textModel = textModel, editMode = False }, Cmd.map DelegateToTextMsg textCmd )



-- SUBSCRIPTIONS


subscriptions : Widget Text.Model Text.Msg -> Model -> ModelElementIdentifier -> Sub Msg
subscriptions textWidget m id =
    Sub.map DelegateToTextMsg <| textWidget.subscriptions m.textModel id



-- VIEW


view : Widget Text.Model Text.Msg -> Model -> Html Msg
view textWidget model =
    if model.editMode then
        span [ onKeyUp [ ( enterKey, UIConfirm ) ] ]
            [ Html.map DelegateToTextMsg <| textWidget.view model.textModel ]
    else
        label [ onDoubleClick UISelect ] [ text <| Text.getContent model.textModel ]