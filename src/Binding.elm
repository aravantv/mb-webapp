module Binding exposing (..)

import ConstraintUtils exposing (Fixes(..), UnfulfillmentInfo, trivialUnfulfillmentInfo)
import DataManager exposing (DataID)
import Widget exposing (ISelectable, Index, Widget, cmdOf, mapParamsSub, mapParamsUp, modelOf)


type BindingResult resType
    = Ok resType
    | Err UnfulfillmentInfo
    | Irrelevant


alwaysOk : (t1 -> t2) -> (t1 -> BindingResult t2)
alwaysOk f x =
    Ok (f x)


ofResult : Result String res -> BindingResult res
ofResult res =
    case res of
        Result.Ok v ->
            Ok v

        Result.Err err ->
            Err (trivialUnfulfillmentInfo err)


map : (res1 -> res2) -> BindingResult res1 -> BindingResult res2
map f res =
    case res of
        Ok v ->
            Ok (f v)

        Err e ->
            Err e

        Irrelevant ->
            Irrelevant


andThen : (res1 -> BindingResult res2) -> BindingResult res1 -> BindingResult res2
andThen f res =
    case res of
        Ok v ->
            f v

        Err e ->
            Err e

        Irrelevant ->
            Irrelevant


type BindingUpInfo carriedValue
    = Set (BindingResult carriedValue)
    | DoNothing


type alias BindingSubInfo carriedValue msg =
    BindingResult carriedValue -> msg


doNothing : a -> ( a, Cmd msg, BindingUpInfo carriedValue )
doNothing x =
    ( x, Cmd.none, DoNothing )


set : a -> carriedValue -> ( a, Cmd msg, BindingUpInfo carriedValue )
set x v =
    ( x, Cmd.none, Set (Ok v) )


type alias BoundWidget model msg carriedValue =
    Widget (BindingUpInfo carriedValue) (BindingSubInfo carriedValue msg) model msg


type alias Binding msg carriedValue =
    { set : carriedValue -> Cmd msg
    , get : (BindingResult carriedValue -> msg) -> Sub msg
    , ask : Cmd msg
    }


applyBinding :
    Binding msg carriedValue
    -> BoundWidget model msg carriedValue
    -> Widget () () model msg
applyBinding b w =
    { init = ( modelOf w.init, Cmd.batch [ cmdOf w.init, b.ask ] )
    , update =
        \msg model ->
            let
                ( newModel, cmd, upInfo ) =
                    w.update msg model

                newCmd =
                    case upInfo of
                        Set (Ok v) ->
                            Cmd.batch [ cmd, b.set v ]

                        _ ->
                            cmd
            in
                ( newModel, newCmd, () )
    , subscriptions =
        \model ->
            let
                ( sub, mapper ) =
                    w.subscriptions model
            in
                ( Sub.batch [ sub, b.get mapper ], () )
    , view = w.view
    }


textBinding : DataID -> Binding msg String
textBinding boundId =
    { get =
        \f ->
            DataManager.getStringSub
                (\id s ->
                    let
                        res =
                            if id == boundId then
                                Ok s
                            else
                                Irrelevant
                    in
                        f res
                )
    , set = \s -> DataManager.setStringCmd boundId s
    , ask = DataManager.askDataCmd boundId
    }


makeBindingWrapper :
    (innerCarriedValue -> BindingResult outerCarriedValue)
    -> (outerCarriedValue -> BindingResult innerCarriedValue)
    -> BoundWidget model msg innerCarriedValue
    -> BoundWidget model msg outerCarriedValue
makeBindingWrapper in2out out2in =
    let
        in2outUp upInfo =
            case upInfo of
                Set v ->
                    case map in2out v of
                        Ok res ->
                            Set res

                        _ ->
                            DoNothing

                DoNothing ->
                    DoNothing
    in
        mapParamsUp in2outUp << mapParamsSub (\subInfo -> subInfo << andThen out2in)


stringOfIntWrapper :
    BoundWidget model msg Int
    -> BoundWidget model msg String
stringOfIntWrapper =
    makeBindingWrapper (alwaysOk toString) (ofResult << String.toInt)


intOfStringWrapper :
    BoundWidget model msg String
    -> BoundWidget model msg Int
intOfStringWrapper =
    makeBindingWrapper (ofResult << String.toInt) (alwaysOk toString)


minus2Wrapper :
    BoundWidget model msg Int
    -> BoundWidget model msg Int
minus2Wrapper =
    makeBindingWrapper (alwaysOk (\n -> n + 2)) (alwaysOk (\n -> n - 2))
