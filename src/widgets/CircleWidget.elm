module CircleWidget exposing (..)

import Html exposing (..)
import Json.Decode
import MetaModel exposing (ModelElementIdentifier, ModelElementSelector)
import Model
import Mouse
import Svg exposing (clipPath, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, rx, ry, stroke, strokeWidth, transform, width)
import Svg.Events
import Widget exposing (IDecision, ISelectable, Index, Widget, cmdOfMsg, doNothing)


type alias Parameters subModel subMsg =
    { wrappedWidget : Widget subModel subMsg
    , selector : ModelElementSelector
    }


createWidget :
    Parameters subModel subMsg
    -> Widget (Model subModel) (Msg subMsg)
createWidget params id =
    { initModel = emptyModel params id
    , initMsg = Init
    , update = update params id
    , subscriptions = subscriptions params id
    , view = view params id
    }



-- MODEL


type alias Model subModel =
    { wrappedModel : subModel
    , cx : Int
    , cy : Int
    , r : Int
    , dragStartPosition : Maybe Mouse.Position
    }


emptyModel : Parameters subModel subMsg -> ModelElementIdentifier -> Model subModel
emptyModel params id =
    { wrappedModel = (params.wrappedWidget id).initModel, cx = 100, cy = 100, r = 50, dragStartPosition = Nothing }



-- UPDATE


type Msg subMsg
    = DelegateToWidget subMsg
    | Init Model.Model
    | StartDragging Mouse.Position
    | Dragging ( Mouse.Position, Mouse.Position )
    | EndDragging


update :
    Parameters subModel subMsg
    -> ModelElementIdentifier
    -> Msg subMsg
    -> Model subModel
    -> ( Model subModel, Cmd (Msg subMsg) )
update params id msg model =
    let
        instantiatedWidget =
            params.wrappedWidget (params.selector id)
    in
        case msg of
            DelegateToWidget subMsg ->
                let
                    ( updatedModel, cmd ) =
                        instantiatedWidget.update subMsg model.wrappedModel
                in
                    ( { model | wrappedModel = updatedModel }, Cmd.map DelegateToWidget cmd )

            Init fi ->
                let
                    initCmd =
                        cmdOfMsg <| DelegateToWidget (instantiatedWidget.initMsg fi)
                in
                    ( emptyModel params id, initCmd )

            StartDragging p ->
                doNothing { model | dragStartPosition = Just p }

            Dragging ( previousPos, newPos ) ->
                doNothing
                    { model
                        | dragStartPosition = Just newPos
                        , cx = model.cx + newPos.x - previousPos.x
                        , cy = model.cy + newPos.y - previousPos.y
                    }

            EndDragging ->
                doNothing { model | dragStartPosition = Nothing }



-- SUBSCRIPTIONS


subscriptions : Parameters subModel subMsg -> ModelElementIdentifier -> Model subModel -> Sub (Msg subMsg)
subscriptions params id model =
    let
        instantiatedWrappedWidget =
            params.wrappedWidget (params.selector id)

        subSub =
            Sub.map DelegateToWidget <| instantiatedWrappedWidget.subscriptions model.wrappedModel
    in
        case model.dragStartPosition of
            Just startPos ->
                Sub.batch <| [ subSub, Mouse.moves (\p -> Dragging ( startPos, p )), Mouse.ups (always EndDragging) ]

            Nothing ->
                subSub



-- VIEW


view : Parameters subModel subMsg -> ModelElementIdentifier -> Model subModel -> Html (Msg subMsg)
view params id model =
    let
        rect attrs =
            Svg.rect ([ rx "5", ry "5", height (toString model.r) ] ++ attrs) []

        onMouseDown msgBuilder =
            Svg.Events.on "mousedown" (Json.Decode.map msgBuilder Mouse.position)
    in
        svg []
            [ Svg.g [ transform <| "translate(" ++ toString model.cx ++ "," ++ toString model.cy ++ ")" ]
                [ Svg.defs []
                    [ Svg.clipPath [ Svg.Attributes.id "id" ]
                        [ rect [ width (toString <| model.r - 6) ] ]
                    ]
                , rect [ width (toString model.r), onMouseDown StartDragging, fill "yellow", stroke "blue", strokeWidth "3" ]
                , Svg.foreignObject
                    [ Svg.Attributes.x "3"
                    , Svg.Attributes.y "3"
                    , Svg.Attributes.clipPath "url(#id)"
                    ]
                    [ Html.map DelegateToWidget <| (params.wrappedWidget (params.selector id)).view model.wrappedModel ]
                ]
            ]
