module NewText exposing (..)

import Html exposing (Html, input, text, label)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onDoubleClick)
import Utils exposing (..)
import SelectableList


widget : SelectableList.NewItemWidget Model Msg
widget =
    { init = wrapModelWithCmd model
    , update = update
    , view = view
    , subscriptions = emptySubscription
    , confirmMsg = Confirm
    , cancelMsg = Cancel
    }



-- MODEL


type alias Model =
    { content : String }


model : Model
model =
    Model ""



-- UPDATE


type Msg
    = Change String
    | Confirm
    | Cancel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newContent ->
            ( { content = newContent }, Cmd.none )

        Confirm ->
            ( model, Cmd.none )

        Cancel ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    input
        [ onInput Change
        , onKeyUp [ ( enterKey, Confirm ), ( escapeKey, Cancel ) ]
        , value model.content
        ]
        []
