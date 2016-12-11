module SelectableText exposing (..)

import Html exposing (Html, Attribute, div, input, text, label)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onDoubleClick)
import Utils exposing (..)


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
    | Edit


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | uiContent = newContent }

        Confirm ->
            { model | editMode = False, content = model.uiContent }

        Cancel ->
            { model | editMode = False, uiContent = model.content }

        Edit ->
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
        label [ onDoubleClick Edit ] [ text model.uiContent ]
