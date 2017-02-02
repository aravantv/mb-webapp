module NewText exposing (..)

import Html exposing (Html, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onDoubleClick, onInput)
import Utils exposing (..)
import Widget exposing (IDecision, TopWidget, UnboundWidget, doNothing)


widget : IDecision Msg (UnboundWidget Model Msg String)
widget =
    { initModel = emptyModel
    , initMsg = Init
    , update = update
    , view = view
    , subscriptions = Widget.emptySubscription
    , confirmMsg = Confirm
    }



-- MODEL


type alias Model =
    String


emptyModel : Model
emptyModel =
    ""



-- UPDATE


type Msg
    = Change String
    | Confirm
    | Cancel
    | Init String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    doNothing <| transform msg model


transform : Msg -> Model -> Model
transform msg model =
    case msg of
        Change newContent ->
            newContent

        Confirm ->
            model

        Cancel ->
            ""

        Init s ->
            s



-- VIEW


view : Model -> Html Msg
view model =
    input
        [ onInput Change
        , onKeyUp [ ( enterKey, Confirm ), ( escapeKey, Cancel ) ]
        , value model
        ]
        []
