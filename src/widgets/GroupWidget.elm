module GroupWidget exposing (..)

import Html exposing (..)
import MetaModel exposing (ModelElementIdentifier, ModelElementSelector)
import Model
import Widget exposing (IDecision, ISelectable, Index, UnboundWidget, Widget, cmdOfMsg, doNothing)


type DivOrSpan
    = Div
    | Span


type alias Parameters subModel1 msg1 subModel2 msg2 =
    { wrappedWidget1 : Widget subModel1 msg1
    , selector1 : ModelElementSelector
    , wrappedWidget2 : Widget subModel2 msg2
    , selector2 : ModelElementSelector
    , divOrSpan : DivOrSpan
    }


createWidget :
    Parameters subModel1 subMsg1 subModel2 subMsg2
    -> Widget (Model subModel1 subModel2) (Msg subMsg1 subMsg2)
createWidget params =
    { initModel = emptyModel params
    , initMsg = \m -> Init ( m, m )
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
    Parameters subModel1 subMsg1 subModel2 subMsg2
    -> Model subModel1 subModel2
emptyModel params =
    { wrappedModel1 = params.wrappedWidget1.initModel
    , wrappedModel2 = params.wrappedWidget2.initModel
    }



-- UPDATE


type Msg subMsg1 subMsg2
    = DelegateToWidget1 subMsg1
    | DelegateToWidget2 subMsg2
    | Init ( Model.Model, Model.Model )


update :
    Parameters subModel1 subMsg1 subModel2 subMsg2
    -> Msg subMsg1 subMsg2
    -> Model subModel1 subModel2
    -> ModelElementIdentifier
    -> ( Model subModel1 subModel2, Cmd (Msg subMsg1 subMsg2) )
update params msg model id =
    case msg of
        DelegateToWidget1 subMsg ->
            let
                ( updatedModel1, cmd ) =
                    params.wrappedWidget1.update subMsg model.wrappedModel1 (params.selector1 id)
            in
                ( { model | wrappedModel1 = updatedModel1 }, Cmd.map DelegateToWidget1 cmd )

        DelegateToWidget2 subMsg ->
            let
                ( updatedModel2, cmd ) =
                    params.wrappedWidget2.update subMsg model.wrappedModel2 (params.selector2 id)
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
    Parameters subModel1 subMsg1 subModel2 subMsg2
    -> Model subModel1 subModel2
    -> ModelElementIdentifier
    -> Sub (Msg subMsg1 subMsg2)
subscriptions params model id =
    let
        sub1 =
            Sub.map DelegateToWidget1 <| params.wrappedWidget1.subscriptions model.wrappedModel1 (params.selector1 id)

        sub2 =
            Sub.map DelegateToWidget2 <| params.wrappedWidget2.subscriptions model.wrappedModel2 (params.selector2 id)
    in
        Sub.batch [ sub1, sub2 ]



-- VIEW


view :
    Parameters subModel1 subMsg1 subModel2 subMsg2
    -> Model subModel1 subModel2
    -> Html (Msg subMsg1 subMsg2)
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
