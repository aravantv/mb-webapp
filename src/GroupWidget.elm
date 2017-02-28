module GroupWidget exposing (..)

import Html exposing (..)
import Widget exposing (Path, IDecision, ISelectable, Index, UnboundWidget, Widget, cmdOfMsg, doNothing)


type alias PathTransformer =
    Path -> Path


type DivOrSpan
    = Div
    | Span


type alias Parameters subModel1 msg1 factoryInput1 subModel2 msg2 factoryInput2 =
    { wrappedWidget1 : Widget subModel1 msg1 factoryInput1
    , pathAdapter1 : PathTransformer
    , wrappedWidget2 : Widget subModel2 msg2 factoryInput2
    , pathAdapter2 : PathTransformer
    , divOrSpan : DivOrSpan
    }


createWidget :
    Parameters subModel1 subMsg1 factoryInput1 subModel2 subMsg2 factoryInput2
    -> Widget (Model subModel1 subModel2) (Msg subMsg1 subMsg2 factoryInput1 factoryInput2) ( factoryInput1, factoryInput2 )
createWidget params =
    { initModel = emptyModel params
    , initMsg = Init
    , update = update params
    , subscriptions = subscriptions params
    , view = view params
    }



-- MODEL


type alias Model subModel1 subModel2 =
    { wrappedModel1 : subModel1
    , wrappedModel2 : subModel2
    }


emptyModel :
    Parameters subModel1 subMsg1 factoryInput1 subModel2 subMsg2 factoryInput2
    -> Model subModel1 subModel2
emptyModel params =
    { wrappedModel1 = params.wrappedWidget1.initModel
    , wrappedModel2 = params.wrappedWidget2.initModel
    }



-- UPDATE


type Msg subMsg1 subMsg2 factoryInput1 factoryInput2
    = DelegateToWidget1 subMsg1
    | DelegateToWidget2 subMsg2
    | Init ( factoryInput1, factoryInput2 )


update :
    Parameters subModel1 subMsg1 factoryInput1 subModel2 subMsg2 factoryInput2
    -> Msg subMsg1 subMsg2 factoryInput1 factoryInput2
    -> Model subModel1 subModel2
    -> Path
    -> ( Model subModel1 subModel2, Cmd (Msg subMsg1 subMsg2 factoryInput1 factoryInput2) )
update params msg model path =
    case msg of
        DelegateToWidget1 subMsg ->
            let
                ( updatedModel1, cmd ) =
                    params.wrappedWidget1.update subMsg model.wrappedModel1 (params.pathAdapter1 path)
            in
                ( { model | wrappedModel1 = updatedModel1 }, Cmd.map DelegateToWidget1 cmd )

        DelegateToWidget2 subMsg ->
            let
                ( updatedModel2, cmd ) =
                    params.wrappedWidget2.update subMsg model.wrappedModel2 (params.pathAdapter2 path)
            in
                ( { model | wrappedModel2 = updatedModel2 }, Cmd.map DelegateToWidget2 cmd )

        Init ( fi1, fi2 ) ->
            let
                initCmd1 =
                    cmdOfMsg <| DelegateToWidget1 (params.wrappedWidget1.initMsg fi1)

                initCmd2 =
                    cmdOfMsg <| DelegateToWidget2 (params.wrappedWidget2.initMsg fi2)
            in
                ( emptyModel params, Cmd.batch [ initCmd1, initCmd2 ] )



-- SUBSCRIPTIONS


subscriptions :
    Parameters subModel1 subMsg1 factoryInput1 subModel2 subMsg2 factoryInput2
    -> Model subModel1 subModel2
    -> Path
    -> Sub (Msg subMsg1 subMsg2 factoryInput1 factoryInput2)
subscriptions params model path =
    let
        sub1 =
            Sub.map DelegateToWidget1 <| params.wrappedWidget1.subscriptions model.wrappedModel1 (params.pathAdapter1 path)

        sub2 =
            Sub.map DelegateToWidget2 <| params.wrappedWidget2.subscriptions model.wrappedModel2 (params.pathAdapter2 path)
    in
        Sub.batch [ sub1, sub2 ]



-- VIEW


view :
    Parameters subModel1 subMsg1 factoryInput1 subModel2 subMsg2 factoryInput2
    -> Model subModel1 subModel2
    -> Html (Msg subMsg1 subMsg2 factoryInput1 factoryInput2)
view params model =
    let
        divOrSpanHtml =
            case params.divOrSpan of
                Div ->
                    div

                Span ->
                    span
    in
        divOrSpanHtml []
            [ Html.map DelegateToWidget1 (params.wrappedWidget1.view model.wrappedModel1)
            , Html.map DelegateToWidget2 (params.wrappedWidget2.view model.wrappedModel2)
            ]
