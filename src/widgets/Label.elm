module Label exposing (..)

import Html exposing (Html, input, label, text)
import Widget exposing (IDecision, Widget, TopWidget, UnboundWidget, doNothing, makeBoundWidget)
import Model


createWidget : String -> Widget Model Msg
createWidget =
    createUnboundWidget >> makeBoundWidget


createUnboundWidget : String -> UnboundWidget Model Msg
createUnboundWidget s =
    { initModel = s
    , initMsg =
        \m ->
            ChangeLabel <|
                case m of
                    Model.String s ->
                        s

                    _ ->
                        ""
    , update = update
    , view = view
    , subscriptions = Widget.emptySubscription
    }



-- MODEL


type alias Model =
    String



-- UPDATE


type Msg
    = ChangeLabel String


update : Msg -> Model -> ( Model, Cmd Msg )
update (ChangeLabel s) model =
    doNothing s



-- VIEW


view : Model -> Html Msg
view =
    text