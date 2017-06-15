module ConstraintService exposing (..)

import ConstraintUtils exposing (ConstraintStatus, UnfulfillmentInfo)
import Data exposing (Data)


type alias ID =
    String


type alias ConstraintID =
    String


type alias ConstraintInstance model msg =
    ( List ID, Constraint model msg )


type alias Constraint state msg =
    { initState : state
    , update : msg -> state -> ( state, Cmd msg )
    , subscriptions : state -> Sub msg
    }


publishStatusChange : ConstraintStatus -> Cmd msg
publishStatusChange s =
    Cmd.none


subscribeDataChange : ID -> (val -> msg) -> Sub msg
subscribeDataChange id _ =
    Sub.none


registerConstraint : Constraint model msg -> Cmd registerMsg
registerConstraint c =
    Cmd.none


registerConstraintInstance : ConstraintInstance model msg -> Cmd msg
registerConstraintInstance ci =
    Cmd.none


publishDataChange : ID -> Data -> Cmd msg
publishDataChange id newVal =
    Cmd.none


subscribeStatusChange : ID -> (List UnfulfillmentInfo -> msg) -> Sub msg
subscribeStatusChange id _ =
    Sub.none
