module CircleWidget exposing (..)

import Html exposing (..)
import Mouse
import Svg exposing (clipPath, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, rx, ry, stroke, strokeWidth, transform, width)
import Svg.Events
import Widget exposing (IDecision, ISelectable, Index, Path, UnboundWidget, Widget, cmdOfMsg, doNothing)
import Json.Decode
import Model


type alias PathTransformer =
    Path -> Path


type alias Parameters subModel subMsg =
    { wrappedWidget : Widget subModel subMsg
    , pathAdapter : PathTransformer
    }


createWidget :
    Parameters subModel subMsg
    -> Widget (Model subModel) (Msg subMsg)
createWidget params =
    { initModel = emptyModel params
    , initMsg = Init
    , update = update params
    , subscriptions = subscriptions params
    , view = view params
    }



-- MODEL


type alias Model subModel =
    { wrappedModel : subModel
    , cx : Int
    , cy : Int
    , r : Int
    , dragStartPosition : Maybe Mouse.Position
    }


emptyModel : Parameters subModel subMsg -> Model subModel
emptyModel params =
    { wrappedModel = params.wrappedWidget.initModel, cx = 100, cy = 100, r = 50, dragStartPosition = Nothing }



-- UPDATE


type Msg subMsg
    = DelegateToWidget subMsg
    | Init Model.Model
    | StartDragging Mouse.Position
    | Dragging ( Mouse.Position, Mouse.Position )
    | EndDragging


update :
    Parameters subModel subMsg
    -> Msg subMsg
    -> Model subModel
    -> Path
    -> ( Model subModel, Cmd (Msg subMsg) )
update params msg model path =
    case msg of
        DelegateToWidget subMsg ->
            let
                ( updatedModel, cmd ) =
                    params.wrappedWidget.update subMsg model.wrappedModel (params.pathAdapter path)
            in
                ( { model | wrappedModel = updatedModel }, Cmd.map DelegateToWidget cmd )

        Init fi ->
            let
                initCmd =
                    cmdOfMsg <| DelegateToWidget (params.wrappedWidget.initMsg fi)
            in
                ( emptyModel params, initCmd )

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


subscriptions : Parameters subModel subMsg -> Model subModel -> Path -> Sub (Msg subMsg)
subscriptions params model path =
    let
        subSub =
            Sub.map DelegateToWidget <| params.wrappedWidget.subscriptions model.wrappedModel (params.pathAdapter path)
    in
        case model.dragStartPosition of
            Just startPos ->
                Sub.batch <| [ subSub, Mouse.moves (\p -> Dragging ( startPos, p )), Mouse.ups (always EndDragging) ]

            Nothing ->
                subSub



-- VIEW


view : Parameters subModel subMsg -> Model subModel -> Html (Msg subMsg)
view params model =
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
                    [ Html.map DelegateToWidget <| params.wrappedWidget.view model.wrappedModel ]
                ]
            ]
