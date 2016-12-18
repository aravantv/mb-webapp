module SelectableText exposing (..)

import Html exposing (Html, input, text, label)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onDoubleClick)
import Utils exposing (..)
import SelectableList


widget : SelectableList.ItemWidget Model Msg
widget =
    { init = wrapModelWithCmd model
    , update = update
    , view = view
    , subscriptions = emptySubscription
    , isSelected = .editMode
    , selectMsg = Select
    , unselectMsg = Unselect
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
    = Change String
    | Unselect
    | Cancel
    | Select


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newContent ->
            ( { model | uiContent = newContent }, Cmd.none )

        Unselect ->
            ( { model | editMode = False, content = model.uiContent }, Cmd.none )

        Cancel ->
            ( { model | uiContent = model.content }, cmdOfMsg Unselect )

        Select ->
            ( { model | editMode = True }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    if model.editMode then
        input
            [ onInput Change
            , onKeyUp [ ( enterKey, Unselect ), ( escapeKey, Cancel ) ]
            , value model.uiContent
            ]
            []
    else
        label [ onDoubleClick Select ] [ text model.uiContent ]
