module Label exposing (..)

import Html exposing (Html, input, label, text)
import Widget exposing (IDecision, TopWidget, Widget, doNothing)


widget : Widget String () () Model Msg
widget =
    { init = \s -> ( s, Cmd.none )
    , update = update
    , view = view
    , subscriptions = \_ -> ( Sub.none, () )
    }



-- MODEL


type alias Model =
    String



-- UPDATE


type Msg
    = ChangeLabel String


update : Msg -> Model -> ( Model, Cmd Msg, () )
update (ChangeLabel s) _ =
    doNothing s



-- VIEW


view : Model -> Html Msg
view =
    text
