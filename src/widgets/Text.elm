module Text exposing (..)

import Binding exposing (..)
import Data exposing (Data(..))
import DataID exposing (DataID)
import Html exposing (Html, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onDoubleClick, onInput)
import Utils exposing (..)
import Widget exposing (ISelectable, Widget, cmdOfMsg, doNothing)


createWidget : GenericBinding Msg String -> Widget Model Msg
createWidget binding id =
    { initMsg = initMsg
    , initModel = emptyModel
    , update = update binding id
    , view = view
    , subscriptions = subscriptions binding id
    }


initMsg : Data -> Msg
initMsg m =
    case m of
        String s ->
            Init s

        _ ->
            NoOp



-- MODEL


type alias Model =
    { initialContent : Maybe String
    , content : String
    , error : Bool
    }


emptyModel : Model
emptyModel =
    { initialContent = Nothing, content = "", error = False }


getContent : Model -> String
getContent =
    .content



-- UPDATE


type Msg
    = UIChange String
    | UICancel
    | ConfirmModel
    | ModelChange (Result BindingErr String)
    | Init String
    | NoOp


update : GenericBinding Msg String -> DataID -> Msg -> Model -> ( Model, Cmd Msg )
update binding id msg model =
    case msg of
        Init s ->
            update binding id (UIChange s) emptyModel

        UIChange newContent ->
            update binding id ConfirmModel { model | content = newContent, error = False }

        ConfirmModel ->
            case (binding id).set model.content of
                Binding.Ok cmd ->
                    ( model, cmd )

                Binding.Err _ ->
                    ( { model | error = True }, Cmd.none )

                Binding.Irrelevant ->
                    ( { model | error = False }, Cmd.none )

        UICancel ->
            case model.initialContent of
                Nothing ->
                    doNothing model

                Just initialContent ->
                    update binding id (UIChange initialContent) model

        ModelChange chgRes ->
            case chgRes of
                Result.Ok newContent ->
                    let
                        newInitialContent =
                            Just <| Maybe.withDefault newContent model.initialContent
                    in
                        doNothing { model | content = newContent, initialContent = newInitialContent, error = False }

                Result.Err err ->
                    doNothing { model | content = "error:" ++ err.description, error = True }

        NoOp ->
            doNothing model



-- SUBSCRIPTIONS


subscriptions : GenericBinding Msg String -> DataID -> Model -> Sub Msg
subscriptions binding id m =
    let
        f bindingRes =
            case bindingRes of
                Binding.Ok s ->
                    ModelChange (Result.Ok s)

                Binding.Err err ->
                    ModelChange (Result.Err err)

                Binding.Irrelevant ->
                    NoOp
    in
        Sub.map f (binding id).get



-- VIEW


view : Model -> Html Msg
view model =
    let
        styleAttr =
            if model.error then
                [ style [ ( "backgroundColor", "red" ) ] ]
            else
                []
    in
        input
            ([ onInput UIChange
             , onKeyUp [ ( escapeKey, UICancel ) ]
             , value model.content
             ]
                ++ styleAttr
            )
            []
