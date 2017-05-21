module Text exposing (..)

import Binding exposing (..)
import ConstraintUtils exposing (UnfulfillmentInfo)
import Data exposing (Data(..))
import Html exposing (Html, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onDoubleClick, onInput)
import Utils exposing (..)


widget : WidgetWithBinding Model Msg String
widget =
    { initMsg = initMsg
    , initModel = emptyModel
    , update = update
    , view = view
    , subscriptions = subscriptions
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
    | ModelChange (Result UnfulfillmentInfo String)
    | Init String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg, BindingUpInfo String )
update msg model =
    case msg of
        Init s ->
            update (UIChange s) emptyModel

        UIChange newContent ->
            update ConfirmModel { model | content = newContent, error = False }

        ConfirmModel ->
            set model model.content

        UICancel ->
            case model.initialContent of
                Nothing ->
                    doNothing model

                Just initialContent ->
                    update (UIChange initialContent) model

        ModelChange chgRes ->
            case chgRes of
                Result.Ok newContent ->
                    let
                        newInitialContent =
                            Just <| Maybe.withDefault newContent model.initialContent
                    in
                        doNothing { model | content = newContent, initialContent = newInitialContent, error = False }

                Result.Err err ->
                    doNothing { model | content = "error:" ++ err.unfulfillmentDescription, error = True }

        NoOp ->
            doNothing model



-- SUBSCRIPTIONS


subscriptions : Model -> ( Sub Msg, BindingSubInfo String Msg )
subscriptions m =
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
        ( Sub.none, f )



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
