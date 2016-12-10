module SelectableText exposing (..)

import Html exposing (Html, Attribute, div, input, text, label)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onDoubleClick, on, keyCode)
import Json.Decode exposing (Decoder, oneOf, fail, succeed, andThen)


-- MODEL


type alias Model =
    { content : String
    , uiContent : String
    , editMode : Bool
    }


model : Model
model =
    Model "" "" True



-- ICI2 plus tard: ne pas faire d'initialisaion mais envoyer des commandes


initModel : String -> Model
initModel content =
    Model content content False



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


onKeyUp : (Int -> Decoder Msg) -> Attribute Msg
onKeyUp decoder =
    on "keyup" (andThen decoder keyCode)


enterKey : number
enterKey =
    13


escapeKey : number
escapeKey =
    27


keyUpDecoder : Int -> Decoder Msg
keyUpDecoder n =
    if n == enterKey then
        succeed Confirm
    else if n == escapeKey then
        succeed Cancel
    else
        fail "key useless for selectable text"


view : Model -> Html Msg
view model =
    div []
        [ if model.editMode then
            input
                [ onInput Change
                , onKeyUp keyUpDecoder
                , value model.uiContent
                ]
                []
          else
            label [ onDoubleClick Edit ] [ text model.uiContent ]
        ]
