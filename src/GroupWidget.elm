module GroupWidget exposing (..)

import Html exposing (..)
import Widget exposing (Path, IDecision, ISelectable, Index, ListBinding, UnboundWidget, Widget, cmdOfMsg, doNothing)


type alias PathTransformer =
    Path -> Path


type DivOrSpan
    = Div
    | Span


htmlOfDivOrSpan : DivOrSpan -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
htmlOfDivOrSpan dos =
    case dos of
        Div ->
            div

        Span ->
            span


type alias Parameters subModel1 msg1 factoryInput1 subModel2 msg2 factoryInput2 =
    ( Widget subModel1 msg1 factoryInput1, PathTransformer, Widget subModel2 msg2 factoryInput2, PathTransformer, DivOrSpan )


createGroupWidget :
    Parameters subModel1 subMsg1 factoryInput1 subModel2 subMsg2 factoryInput2
    -> Widget (Model subModel1 subModel2) (Msg subMsg1 subMsg2 factoryInput1 factoryInput2) ( factoryInput1, factoryInput2 )
createGroupWidget params =
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
emptyModel ( wrappedWidget1, _, wrappedWidget2, _, _ ) =
    { wrappedModel1 = wrappedWidget1.initModel
    , wrappedModel2 = wrappedWidget2.initModel
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
    let
        ( wrappedWidget1, pathAdapter1, wrappedWidget2, pathAdapter2, _ ) =
            params
    in
        case msg of
            DelegateToWidget1 subMsg ->
                let
                    ( updatedModel1, cmd ) =
                        wrappedWidget1.update subMsg model.wrappedModel1 (pathAdapter1 path)
                in
                    ( { model | wrappedModel1 = updatedModel1 }, Cmd.map DelegateToWidget1 cmd )

            DelegateToWidget2 subMsg ->
                let
                    ( updatedModel2, cmd ) =
                        wrappedWidget2.update subMsg model.wrappedModel2 (pathAdapter2 path)
                in
                    ( { model | wrappedModel2 = updatedModel2 }, Cmd.map DelegateToWidget2 cmd )

            Init ( fi1, fi2 ) ->
                let
                    initCmd1 =
                        cmdOfMsg <| DelegateToWidget1 (wrappedWidget1.initMsg fi1)

                    initCmd2 =
                        cmdOfMsg <| DelegateToWidget2 (wrappedWidget2.initMsg fi2)
                in
                    ( emptyModel params, Cmd.batch [ initCmd1, initCmd2 ] )



-- SUBSCRIPTIONS


subscriptions :
    Parameters subModel1 subMsg1 factoryInput1 subModel2 subMsg2 factoryInput2
    -> Model subModel1 subModel2
    -> Path
    -> Sub (Msg subMsg1 subMsg2 factoryInput1 factoryInput2)
subscriptions ( wrappedWidget1, pathAdapter1, wrappedWidget2, pathAdapter2, _ ) model path =
    let
        sub1 =
            Sub.map DelegateToWidget1 <| wrappedWidget1.subscriptions model.wrappedModel1 (pathAdapter1 path)

        sub2 =
            Sub.map DelegateToWidget2 <| wrappedWidget2.subscriptions model.wrappedModel2 (pathAdapter2 path)
    in
        Sub.batch [ sub1, sub2 ]



-- VIEW


view :
    Parameters subModel1 subMsg1 factoryInput1 subModel2 subMsg2 factoryInput2
    -> Model subModel1 subModel2
    -> Html (Msg subMsg1 subMsg2 factoryInput1 factoryInput2)
view ( wrappedWidget1, _, wrappedWidget2, _, dos ) model =
    htmlOfDivOrSpan dos
        []
        [ Html.map DelegateToWidget1 (wrappedWidget1.view model.wrappedModel1)
        , Html.map DelegateToWidget2 (wrappedWidget2.view model.wrappedModel2)
        ]
