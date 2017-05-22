module Widget exposing (..)

import Data exposing (Data)
import DataID exposing (DataID)
import Html exposing (Html)
import Task


type alias Widget upInfo subInfo model msg =
    { initModel : model
    , initMsg : Data -> msg
    , update : msg -> model -> ( model, Cmd msg, upInfo )
    , subscriptions : model -> ( Sub msg, subInfo )
    , view : model -> Html msg
    }


mapParamsUp :
    (upInfo1 -> upInfo2)
    -> Widget upInfo1 paramsSub model msg
    -> Widget upInfo2 paramsSub model msg
mapParamsUp fUp w =
    { initModel = w.initModel
    , initMsg = w.initMsg
    , update =
        \msg model ->
            let
                ( newModel, cmd, upInfo ) =
                    w.update msg model
            in
                ( newModel, cmd, fUp upInfo )
    , subscriptions = w.subscriptions
    , view = w.view
    }


mapParamsSub :
    (subInfo1 -> subInfo2)
    -> Widget paramsUp subInfo1 model msg
    -> Widget paramsUp subInfo2 model msg
mapParamsSub fSub w =
    { initModel = w.initModel
    , initMsg = w.initMsg
    , update = w.update
    , subscriptions =
        \model ->
            let
                ( sub, subInfo ) =
                    w.subscriptions model
            in
                ( sub, fSub subInfo )
    , view = w.view
    }


type alias WidgetTransformer upInfo1 subInfo1 model1 msg1 upInfo2 subInfo2 model2 msg2 =
    Widget upInfo1 subInfo1 model1 msg1 -> Widget upInfo2 subInfo2 model2 msg2


type alias TopWidget model msg =
    DataID
    -> { init : ( model, Cmd msg )
       , update : msg -> model -> ( model, Cmd msg )
       , subscriptions : model -> Sub msg
       , view : model -> Html msg
       }



-- Row types are used only to compose new types in a modular fashion,
-- *not* to make functions accept abstract structures:
-- just use wrappers if you do not have the precise concrete type


type alias ISelectable model msg base =
    { base
        | isSelected : model -> Bool
        , selectMsg : msg
        , unselectMsg : msg
    }


type alias IDecision msg base =
    { base | confirmMsg : msg }


type alias Index =
    Int


wrapUpdateWithCmd : (msg -> model -> model) -> msg -> model -> ( model, Cmd msg )
wrapUpdateWithCmd update =
    \msg model -> update msg model ! []


doNothing : model -> ( model, Cmd msg, () )
doNothing m =
    ( m, Cmd.none, () )


cmdOfMsg : msg -> Cmd msg
cmdOfMsg msg =
    Task.perform identity (Task.succeed msg)


type alias Factory widgetModel =
    widgetModel -> Data
