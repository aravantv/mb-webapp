module Text exposing (..)

import Binding exposing (..)
import Html exposing (Html, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onDoubleClick, onInput)
import MetaModel exposing (ModelElementIdentifier)
import Model
import Utils exposing (..)
import Widget exposing (ISelectable, Widget, cmdOfMsg, doNothing)


createWidget : Binding Msg String -> Widget Model Msg
createWidget binding =
    { initMsg = initMsg
    , initModel = emptyModel
    , update = update binding
    , view = view
    , subscriptions = subscriptions binding
    }


initMsg : Model.Model -> Msg
initMsg m =
    case m of
        Model.String s ->
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


update : Binding Msg String -> Msg -> Model -> ModelElementIdentifier -> ( Model, Cmd Msg )
update binding msg model id =
    case msg of
        Init s ->
            update binding (UIChange s) emptyModel id

        UIChange newContent ->
            update binding ConfirmModel { model | content = newContent, error = False } id

        ConfirmModel ->
            case binding.set id model.content of
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
                    update binding (UIChange initialContent) model id

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


subscriptions : Binding Msg String -> Model -> ModelElementIdentifier -> Sub Msg
subscriptions binding m id =
    let
        f bindingRes =
            case bindingRes of
                Binding.Ok v ->
                    ModelChange (Result.Ok v)

                Binding.Err err ->
                    ModelChange (Result.Err err)

                Binding.Irrelevant ->
                    NoOp
    in
        Sub.map f (binding.get id)



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
