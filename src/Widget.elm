module Widget exposing (..)

import Data exposing (Data)
import DataID exposing (DataID)
import Html exposing (Html)
import Task


type alias Widget paramsUp paramsSub model msg =
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg, paramsUp )
    , subscriptions : model -> ( Sub msg, paramsSub )
    , view : model -> Html msg
    }


modelOf : ( model, rest ) -> model
modelOf ( m, _ ) =
    m


cmdOf : ( rest, Cmd msg ) -> Cmd msg
cmdOf ( _, c ) =
    c


mapParamsUp :
    (paramsUp1 -> paramsUp2)
    -> Widget paramsUp1 paramsSub model msg
    -> Widget paramsUp2 paramsSub model msg
mapParamsUp fUp w =
    { init = w.init
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
    (paramsSub1 -> paramsSub2)
    -> Widget paramsUp paramsSub1 model msg
    -> Widget paramsUp paramsSub2 model msg
mapParamsSub fSub w =
    { init = w.init
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
