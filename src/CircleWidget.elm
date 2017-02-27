module CircleWidget exposing (..)

import Html exposing (..)
import Mouse
import Svg exposing (clipPath, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, rx, ry, stroke, strokeWidth, transform, width)
import Svg.Events exposing (onMouseDown)
import Widget exposing (IDecision, ISelectable, Index, Path, UnboundWidget, Widget, cmdOfMsg, doNothing)


type alias PathTransformer =
    Path -> Path


type alias Parameters subModel subMsg factoryInput =
    { wrappedWidget : Widget subModel subMsg factoryInput
    , pathAdapter : PathTransformer
    }


createCircleWidget :
    Parameters subModel subMsg factoryInput
    -> Widget (Model subModel) (Msg subMsg factoryInput) factoryInput
createCircleWidget params =
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
    , dragging : Bool
    }


emptyModel : Parameters subModel subMsg factoryInput -> Model subModel
emptyModel params =
    { wrappedModel = params.wrappedWidget.initModel, cx = 100, cy = 100, r = 50, dragging = False }



-- UPDATE


type Msg subMsg factoryInput
    = DelegateToWidget subMsg
    | Init factoryInput
    | StartDragging
    | Dragging Mouse.Position
    | EndDragging


update :
    Parameters subModel subMsg factoryInput
    -> Msg subMsg factoryInput
    -> Model subModel
    -> Path
    -> ( Model subModel, Cmd (Msg subMsg factoryInput) )
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

        StartDragging ->
            doNothing { model | dragging = True }

        Dragging p ->
            doNothing { model | cx = p.x, cy = p.y }

        EndDragging ->
            doNothing { model | dragging = False }



-- SUBSCRIPTIONS


subscriptions :
    Parameters subModel subMsg factoryInput
    -> Model subModel
    -> Path
    -> Sub (Msg subMsg factoryInput)
subscriptions params model path =
    let
        subSub =
            Sub.map DelegateToWidget <| params.wrappedWidget.subscriptions model.wrappedModel (params.pathAdapter path)
    in
        if model.dragging then
            Sub.batch <| subSub :: [ Mouse.moves Dragging, Mouse.ups (always EndDragging) ]
        else
            subSub



-- VIEW


view : Parameters subModel subMsg factoryInput -> Model subModel -> Html (Msg subMsg factoryInput)
view params model =
    let
        rect attrs =
            Svg.rect ([ rx "5", ry "5", height (toString model.r) ] ++ attrs) []
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
