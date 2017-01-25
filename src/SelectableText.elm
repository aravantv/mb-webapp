module SelectableText exposing (..)

import Html exposing (Html, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onDoubleClick, onInput)
import Utils exposing (..)
import Widget exposing (Binding, ISelectable, Path, Widget, cmdOfMsg, wrapWithNoCmd)


createWidget : Binding Msg String err -> ISelectable Model Msg (Widget Model Msg)
createWidget binding =
    { init = \p -> wrapWithNoCmd model
    , update = update binding
    , view = view
    , subscriptions = subscriptions binding
    , isSelected = .editMode
    , selectMsg = UISelect
    , unselectMsg = UIConfirm
    }



-- MODEL


type alias Model =
    { content : String
    , uiContent : String
    , editMode : Bool
    }


model : Model
model =
    Model "" "" True



-- UPDATE


type Msg
    = UIChange String
    | UIConfirm
    | UICancel
    | UISelect
    | ModelChange String
    | NoOp



-- in the long run, the parameter of ModelChange should not be a Maybe String but just a String


update : Binding Msg String err -> Path -> Msg -> Model -> ( Model, Cmd Msg )
update binding p msg model =
    case msg of
        UIChange newContent ->
            wrapWithNoCmd { model | uiContent = newContent }

        UIConfirm ->
            ( { model | editMode = False, content = model.uiContent }, binding.set p model.uiContent )

        UICancel ->
            ( { model | uiContent = model.content }, cmdOfMsg UIConfirm )

        UISelect ->
            wrapWithNoCmd { model | editMode = True }

        NoOp ->
            wrapWithNoCmd model

        ModelChange newContent ->
            wrapWithNoCmd { model | uiContent = newContent, content = newContent }



-- SUBSCRIPTIONS


subscriptions : Binding Msg String err -> Path -> Model -> Sub Msg
subscriptions binding p m =
    Sub.map (Result.withDefault NoOp << Result.map ModelChange) (binding.get p)



-- VIEW


view : Model -> Html Msg
view model =
    if model.editMode then
        input
            [ onInput UIChange
            , onKeyUp [ ( enterKey, UIConfirm ), ( escapeKey, UICancel ) ]
            , value model.uiContent
            ]
            []
    else
        label [ onDoubleClick UISelect ] [ text model.uiContent ]
