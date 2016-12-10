module Main exposing (..)

import Html exposing (Html, Attribute, div, input, text, label, ul, li)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onDoubleClick, on, keyCode)
import Json.Decode exposing (Decoder, oneOf, fail, succeed, andThen)
import SelectableText


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { uiToAdd : String, contents : List SelectableText.Model }


model : Model
model =
    { uiToAdd = "", contents = [] }



-- UPDATE


type Msg
    = Add
    | ChangeToAdd String
    | WidgetMsg SelectableText.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add ->
            { uiToAdd = ""
            , contents = SelectableText.initModel model.uiToAdd :: model.contents
            }

        ChangeToAdd s ->
            { model | uiToAdd = s }

        WidgetMsg msg ->
            { model | contents = List.map (SelectableText.update msg) model.contents }



-- VIEW


onKeyUp : (Int -> Decoder Msg) -> Attribute Msg
onKeyUp decoder =
    on "keyup" (andThen decoder keyCode)


enterKey : number
enterKey =
    13


keyUpDecoder : Int -> Decoder Msg
keyUpDecoder n =
    if n == enterKey then
        succeed Add
    else
        fail "key useless for selectable text"


view : Model -> Html Msg
view model =
    div []
        [ input
            [ onInput ChangeToAdd
            , onKeyUp keyUpDecoder
            , value model.uiToAdd
            ]
            []
        , ul [] <| List.map (\x -> li [] [ Html.map WidgetMsg <| SelectableText.view x ]) model.contents
        ]
