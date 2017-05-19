module NewText exposing (..)

import Data exposing (Data(..))
import Html exposing (Html, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onDoubleClick, onInput)
import Utils exposing (..)
import Widget exposing (BoundWidget, IDecision, TopWidget, Unbound, Widget)


widget : IDecision Msg { widget : Widget () () Model Msg }
widget =
    { confirmMsg = Confirm
    , widget =
        \_ ->
            { initModel = emptyModel
            , initMsg = Init
            , update = update
            , view = view
            , subscriptions = \_ -> ( Sub.none, () )
            }
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
    | Init Data


update : Msg -> Model -> ( Model, Cmd Msg, () )
update msg model =
    ( transform msg model, Cmd.none, () )


transform : Msg -> Model -> Model
transform msg model =
    case msg of
        Change newContent ->
            newContent

        Confirm ->
            model

        Cancel ->
            ""

        Init m ->
            case m of
                String s ->
                    s

                _ ->
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
