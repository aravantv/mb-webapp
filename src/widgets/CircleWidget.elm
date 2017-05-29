module CircleWidget exposing (..)

import Html exposing (..)
import Json.Decode
import Mouse
import Svg exposing (clipPath, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, rx, ry, stroke, strokeWidth, transform, width)
import Svg.Events
import Widget exposing (IDecision, ISelectable, Index, Widget, cmdOf, cmdOfMsg, doNothing, modelOf)


type alias Parameters subModel subMsg =
    Widget () () subModel subMsg


createWidget :
    Parameters subModel subMsg
    -> Widget () () (Model subModel) (Msg subMsg)
createWidget params =
    { init = init params
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


init : Parameters subModel subMsg -> ( Model subModel, Cmd (Msg subMsg) )
init wrappedWidget =
    ( { wrappedModel = modelOf wrappedWidget.init
      , cx = 100
      , cy = 100
      , r = 100
      , dragStartPosition = Nothing
      }
    , Cmd.map DelegateToWidget (cmdOf wrappedWidget.init)
    )



-- UPDATE


type Msg subMsg
    = DelegateToWidget subMsg
    | StartDragging Mouse.Position
    | Dragging ( Mouse.Position, Mouse.Position )
    | EndDragging


update :
    Parameters subModel subMsg
    -> Msg subMsg
    -> Model subModel
    -> ( Model subModel, Cmd (Msg subMsg), () )
update wrappedWidget msg model =
    case msg of
        DelegateToWidget subMsg ->
            let
                ( updatedModel, cmd, () ) =
                    wrappedWidget.update subMsg model.wrappedModel
            in
                ( { model | wrappedModel = updatedModel }, Cmd.map DelegateToWidget cmd, () )

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


subscriptions : Parameters subModel subMsg -> Model subModel -> ( Sub (Msg subMsg), () )
subscriptions wrappedWidget model =
    let
        ( subSub, () ) =
            wrappedWidget.subscriptions model.wrappedModel

        subRes =
            case model.dragStartPosition of
                Just startPos ->
                    Sub.batch <|
                        [ Sub.map DelegateToWidget subSub
                        , Mouse.moves (\p -> Dragging ( startPos, p ))
                        , Mouse.ups (always EndDragging)
                        ]

                Nothing ->
                    Sub.map DelegateToWidget subSub
    in
        ( subRes, () )



-- VIEW


view : Parameters subModel subMsg -> Model subModel -> Html (Msg subMsg)
view wrappedWidget model =
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
                , rect [ width (toString model.r), onMouseDown StartDragging, fill "lightblue", stroke "lightskyblue", strokeWidth "3" ]
                , Svg.foreignObject
                    [ Svg.Attributes.x "3"
                    , Svg.Attributes.y "3"
                    , Svg.Attributes.clipPath "url(#id)"
                    ]
                    [ Html.map DelegateToWidget <| wrappedWidget.view model.wrappedModel ]
                ]
            ]
