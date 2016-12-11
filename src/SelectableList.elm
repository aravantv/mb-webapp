module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import SelectableText as Widget
import Utils exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = model ! []
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
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
    | Remove WidgetIndex


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add ->
            { uiToAdd = ""
            , contents = Widget.model :: model.contents
            }
                ! List.map (syncCmd << WidgetMsg 0) [ Widget.Confirm, Widget.Change model.uiToAdd ]

        ChangeToAdd s ->
            { model | uiToAdd = s } ! []

        WidgetMsg i msg ->
            { model | contents = List.indexedMap (updateWidget i msg) model.contents }
                ! if msg == Widget.Edit then
                    List.map (\j -> syncCmd <| WidgetMsg j <| Widget.Confirm)
                        (List.range 0 (i - 1) ++ List.range (i + 1) (List.length model.contents))
                  else
                    []

        Remove i ->
            let
                before_i =
                    List.take i model.contents

                after_i =
                    List.drop (i + 1) model.contents
            in
                { model | contents = before_i ++ after_i } ! []


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
    li []
        [ span []
            [ Html.map (WidgetMsg i) <| Widget.view m
            , button [ onClick <| Remove i ] [ text "-" ]
            ]
        ]
