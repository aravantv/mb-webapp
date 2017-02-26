module Text exposing (..)

import Html exposing (Html, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onDoubleClick, onInput)
import Utils exposing (..)
import Widget exposing (Binding, ISelectable, Path, Widget, cmdOfMsg, doNothing)


createWidget : Binding Msg String err -> Widget Model Msg String
createWidget binding =
    { initMsg = Init
    , initModel = emptyModel
    , update = update binding
    , view = view
    , subscriptions = subscriptions binding
    }



-- MODEL


type alias Model =
    { initialContent : Maybe String
    , content : String
    }


emptyModel : Model
emptyModel =
    { initialContent = Nothing, content = "" }


getContent : Model -> String
getContent =
    .content



-- UPDATE


type Msg
    = UIChange String
    | UICancel
    | ConfirmModel
    | ModelChange String
    | Init String
    | NoOp


update : Binding Msg String err -> Msg -> Model -> Path -> ( Model, Cmd Msg )
update binding msg model p =
    case msg of
        Init s ->
            ( emptyModel, binding.set p s )

        UIChange newContent ->
            update binding ConfirmModel { model | content = newContent } p

        ConfirmModel ->
            ( model, binding.set p model.content )

        UICancel ->
            case model.initialContent of
                Nothing ->
                    doNothing model

                Just initialContent ->
                    update binding ConfirmModel { model | content = initialContent } p

        ModelChange newContent ->
            let
                newInitialContent =
                    Just <| Maybe.withDefault newContent model.initialContent
            in
                doNothing { model | content = newContent, initialContent = newInitialContent }

        NoOp ->
            doNothing model



-- SUBSCRIPTIONS


subscriptions : Binding Msg String err -> Model -> Path -> Sub Msg
subscriptions binding m p =
    Sub.map (Result.withDefault NoOp << Result.map ModelChange) (binding.get p)



-- VIEW


view : Model -> Html Msg
view model =
    input
        [ onInput UIChange
        , onKeyUp [ ( escapeKey, UICancel ) ]
        , value model.content
        ]
        []
