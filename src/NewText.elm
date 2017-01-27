module NewText exposing (..)

import Html exposing (Html, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onDoubleClick, onInput)
import Utils exposing (..)
import Widget exposing (IDecision, TopWidget)


widget : IDecision Msg (TopWidget Model Msg)
widget =
    { init = Widget.wrapWithNoCmd model
    , update = update
    , view = view
    , subscriptions = Widget.emptySubscription
    , confirmMsg = Confirm
    }



-- MODEL


type alias Model =
    String


model : Model
model =
    ""



-- UPDATE


type Msg
    = Change String
    | Confirm
    | Cancel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newContent ->
            ( newContent, Cmd.none )

        Confirm ->
            ( model, Cmd.none )

        Cancel ->
            ( "", Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    input
        [ onInput Change
        , onKeyUp [ ( enterKey, Confirm ), ( escapeKey, Cancel ) ]
        , value model
        ]
        []
