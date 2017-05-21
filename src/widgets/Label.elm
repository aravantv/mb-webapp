module Label exposing (..)

import Data exposing (Data(..))
import Html exposing (Html, input, label, text)
import Widget exposing (IDecision, TopWidget, Widget, doNothing)


createWidget : String -> Widget () () Model Msg
createWidget s =
    { initModel = s
    , initMsg =
        \m ->
            ChangeLabel <|
                case m of
                    String s ->
                        s

                    _ ->
                        ""
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
