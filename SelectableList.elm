module Main exposing (..)

import Html exposing (Html, Attribute, div, input, text, label, ul, li)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onDoubleClick, on, keyCode)
import Json.Decode exposing (Decoder, oneOf, fail, succeed, andThen)
import SelectableText as Widget


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { uiToAdd : String, contents : List Widget.Model }


model : Model
model =
    { uiToAdd = "", contents = [] }



-- UPDATE


type alias WidgetIndex =
    Int


type Msg
    = Add
    | ChangeToAdd String
    | WidgetMsg WidgetIndex Widget.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add ->
            { uiToAdd = ""
            , contents = Widget.initModel model.uiToAdd :: model.contents
            }

        ChangeToAdd s ->
            { model | uiToAdd = s }

        WidgetMsg i msg ->
            { model | contents = List.indexedMap (updateWidget i msg) model.contents }


updateWidget : WidgetIndex -> Widget.Msg -> WidgetIndex -> Widget.Model -> Widget.Model
updateWidget refIndex msg candidateIndex model =
    if candidateIndex == refIndex then
        Widget.update msg model
    else
        model



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
        , ul [] <| List.indexedMap viewWidget model.contents
        ]


viewWidget : WidgetIndex -> Widget.Model -> Html Msg
viewWidget i m =
    li [] [ Html.map (WidgetMsg i) <| Widget.view m ]
