module SelectableText exposing (..)

import Html exposing (Html, input, text, label)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onDoubleClick)
import Utils exposing (..)
import SelectableList


widget : SelectableList.ItemWidget Model Msg
widget =
    { init = wrapModelWithCmd model
    , update = wrapUpdateWithCmd update
    , view = view
    , subscriptions = emptySubscription
    , isSelected = .editMode
    , selectMsg = Select
    , unselectMsg = Confirm
    , change = \s -> Change s
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
    | Confirm
    | Cancel
    | Select


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | uiContent = newContent }

        Confirm ->
            { model | editMode = False, content = model.uiContent }

        Cancel ->
            { model | editMode = False, uiContent = model.content }

        Select ->
            { model | editMode = True, uiContent = model.content }



-- VIEW


view : Model -> Html Msg
view model =
    if model.editMode then
        input
            [ onInput Change
            , onKeyUp [ ( enterKey, Confirm ), ( escapeKey, Cancel ) ]
            , value model.uiContent
            ]
            []
    else
        label [ onDoubleClick Select ] [ text model.uiContent ]
