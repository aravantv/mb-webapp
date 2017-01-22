module SelectableText exposing (..)

import Html exposing (Html, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onDoubleClick, onInput)
import Utils exposing (..)
import Widget exposing (Binding, ISelectable, FixedPathBinding, Widget, cmdOfMsg, wrapModelWithCmd)


createWidget : Binding Msg String err -> Widget.Path -> ISelectable Model Msg (Widget Model Msg)
createWidget binding p =
    let
        concreteBinding =
            binding p
    in
        { init = wrapModelWithCmd model
        , update = update concreteBinding
        , view = view
        , subscriptions = subscriptions concreteBinding
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


model : Model
model =
    Model "" "" True



-- UPDATE


type Msg
    = UIChange String
    | UIConfirm
    | UICancel
    | UISelect
    | ModelChange String
    | NoOp



-- in the long run, the parameter of ModelChange should not be a Maybe String but just a String


update : FixedPathBinding Msg String err -> Msg -> Model -> ( Model, Cmd Msg )
update binding msg model =
    case msg of
        UIChange newContent ->
            ( { model | uiContent = newContent }, Cmd.none )

        UIConfirm ->
            ( { model | editMode = False, content = model.uiContent }, binding.set model.uiContent )

        UICancel ->
            ( { model | uiContent = model.content }, cmdOfMsg UIConfirm )

        UISelect ->
            ( { model | editMode = True }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        ModelChange newContent ->
            ( { model | uiContent = newContent, content = newContent }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : FixedPathBinding Msg String err -> Model -> Sub Msg
subscriptions binding model =
    Sub.map (Result.withDefault NoOp << Result.map ModelChange) binding.get



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
