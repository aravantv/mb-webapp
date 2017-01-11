module SelectableText exposing (..)

import Html exposing (Html, input, text, label)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onDoubleClick)
import Utils exposing (..)
import Widget exposing (wrapModelWithCmd, cmdOfMsg)


createWidget : Widget.Binding Msg String -> Widget.SelectableWidget Model Msg
createWidget binding =
    { init = wrapModelWithCmd model
    , update = update binding
    , view = view
    , subscriptions = \_ -> Sub.map Change binding.get
    , isSelected = .editMode
    , selectMsg = Select
    , unselectMsg = Confirm
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


update : Widget.Binding Msg String -> Msg -> Model -> ( Model, Cmd Msg )
update binding msg model =
    case msg of
        Change newContent ->
            ( { model | uiContent = newContent }, Cmd.none )

        Confirm ->
            ( { model | editMode = False, content = model.uiContent }, binding.set model.uiContent )

        Cancel ->
            ( { model | uiContent = model.content }, cmdOfMsg Confirm )

        Select ->
            ( { model | editMode = True }, Cmd.none )



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
