module NewText exposing (..)

import Html exposing (Html, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onDoubleClick, onInput)
import Utils exposing (..)
import Widget exposing (IDecision, TopWidget, Widget, doNothing)


widget : IDecision Msg { widget : Widget String () () Model Msg }
widget =
    { confirmMsg = Confirm
    , widget =
        { init = \s -> ( s, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> ( Sub.none, () )
        }
    }



-- MODEL


type alias Model =
    String



-- UPDATE


type Msg
    = Change String
    | Confirm
    | Cancel


update : Msg -> Model -> ( Model, Cmd Msg, () )
update msg model =
    doNothing (transform msg model)


transform : Msg -> Model -> Model
transform msg model =
    case msg of
        Change newContent ->
            newContent

        Confirm ->
            model

        Cancel ->
            ""



-- VIEW


view : Model -> Html Msg
view model =
    input
        [ onInput Change
        , onKeyUp [ ( enterKey, Confirm ), ( escapeKey, Cancel ) ]
        , value model
        ]
        []
