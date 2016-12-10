module Main exposing (..)

import Html exposing (Html, Attribute, div, input, text, label, ul, li)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import SelectableText as Widget
import Utils exposing (..)


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


view : Model -> Html Msg
view model =
    ul [] <|
        (li [] [ viewToAddField model ])
            :: List.indexedMap viewWidget model.contents


viewToAddField : Model -> Html Msg
viewToAddField model =
    input
        [ onInput ChangeToAdd
        , onKeyUp [ ( enterKey, Add ) ]
        , value model.uiToAdd
        ]
        []


viewWidget : WidgetIndex -> Widget.Model -> Html Msg
viewWidget i m =
    li [] [ Html.map (WidgetMsg i) <| Widget.view m ]
