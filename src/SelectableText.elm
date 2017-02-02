module SelectableText exposing (..)

import Html exposing (Html, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onDoubleClick, onInput)
import Utils exposing (..)
import Widget exposing (Binding, ISelectable, Path, Widget, cmdOfMsg, doNothing)


createWidget : Binding Msg String err -> ISelectable Model Msg (Widget Model Msg String)
createWidget binding =
    { initMsg = Init
    , initModel = emptyModel
    , update = update binding
    , view = view
    , subscriptions = subscriptions binding
    , isSelected = .editMode
    , selectMsg = UISelect
    , unselectMsg = UIConfirm
    }



-- MODEL


type alias Model =
    { content : String
    , uiContent : String
    , editMode : Bool
    }


emptyModel : Model
emptyModel =
    { content = "", uiContent = "", editMode = False }



-- UPDATE


type Msg
    = UIChange String
    | UIConfirm
    | UICancel
    | UISelect
    | ModelChange String
    | Init String
    | NoOp


update : Binding Msg String err -> Msg -> Model -> Path -> ( Model, Cmd Msg )
update binding msg model p =
    case msg of
        Init s ->
            ( emptyModel, binding.set p s )

        UIChange newContent ->
            { model | uiContent = newContent } |> doNothing

        UIConfirm ->
            ( { model | editMode = False, content = model.uiContent }, binding.set p model.content )

        UICancel ->
            { model | uiContent = model.content, editMode = False } |> doNothing

        UISelect ->
            { model | editMode = True } |> doNothing

        ModelChange newContent ->
            { model | uiContent = newContent, content = newContent } |> doNothing

        NoOp ->
            doNothing model


applyUIChange : String -> Model -> Model
applyUIChange newContent m =
    { m | uiContent = newContent }



-- SUBSCRIPTIONS


subscriptions : Binding Msg String err -> Model -> Path -> Sub Msg
subscriptions binding m p =
    Sub.map (Result.withDefault NoOp << Result.map ModelChange) (binding.get p)



-- VIEW


view : Model -> Html Msg
view model =
    if model.editMode then
        input
            [ onInput UIChange
            , onKeyUp [ ( enterKey, UIConfirm ), ( escapeKey, UICancel ) ]
            , value model.uiContent
            ]
            []
    else
        label [ onDoubleClick UISelect ] [ text model.uiContent ]
