module Text exposing (..)

import Html exposing (Html, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onDoubleClick, onInput)
import Utils exposing (..)
import Widget exposing (Binding, ISelectable, Path, Widget, cmdOfMsg, doNothing)


createWidget : Binding Msg String err -> Widget Model Msg String
createWidget binding =
    { initMsg = Init
    , initModel = emptyModel
    , update = update binding
    , view = view
    , subscriptions = subscriptions binding
    }



-- MODEL


type alias Model =
    { content : String
    , uiContent : String
    }


emptyModel : Model
emptyModel =
    { content = "", uiContent = "" }



-- UPDATE


type Msg
    = UIChange String
    | UIConfirm
    | UICancel
    | ModelChange String
    | Init String
    | NoOp


update : Binding Msg String err -> Msg -> Model -> Path -> ( Model, Cmd Msg )
update binding msg model p =
    case msg of
        Init s ->
            ( emptyModel, binding.set p s )

        UIChange newContent ->
            doNothing { model | uiContent = newContent }

        UIConfirm ->
            ( { model | content = model.uiContent }, binding.set p model.uiContent )

        UICancel ->
            doNothing { model | uiContent = model.content }

        ModelChange newContent ->
            doNothing { model | uiContent = newContent, content = newContent }

        NoOp ->
            doNothing model



-- SUBSCRIPTIONS


subscriptions : Binding Msg String err -> Model -> Path -> Sub Msg
subscriptions binding m p =
    Sub.map (Result.withDefault NoOp << Result.map ModelChange) (binding.get p)



-- VIEW


view : Model -> Html Msg
view model =
    input
        [ onInput UIChange
        , onKeyUp [ ( enterKey, UIConfirm ), ( escapeKey, UICancel ) ]
        , value model.uiContent
        ]
        []
