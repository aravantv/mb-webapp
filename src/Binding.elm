module Binding exposing (..)

import ConstraintUtils exposing (Fixes(..), UnfulfillmentInfo, trivialUnfulfillmentInfo)
import DataID exposing (DataID, getItemIdentifier, isItemOf, itemOf)
import DataManager
import Widget exposing (ISelectable, Index, Widget, WidgetTransformer, mapParamsSub, mapParamsUp)


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


type alias WidgetWithBinding model msg carriedValue =
    Widget (BindingUpInfo carriedValue) (BindingSubInfo carriedValue msg) model msg


type alias Binding msg carriedValue =
    { set : carriedValue -> Cmd msg
    , get : (BindingResult carriedValue -> msg) -> Sub msg
    }


applyBinding :
    Binding msg carriedValue
    -> Widget (BindingUpInfo carriedValue) (BindingSubInfo carriedValue msg) model msg
    -> Widget () () model msg
applyBinding b w =
    { init = w.init
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
                (\( id, s ) ->
                    let
                        res =
                            if id == boundId then
                                Ok s
                            else
                                Irrelevant
                    in
                        f res
                )
    , set = \s -> DataManager.setStringCmd ( boundId, s )
    }


makeBindingWrapper :
    (innerCarriedValue -> BindingResult outerCarriedValue)
    -> (outerCarriedValue -> BindingResult innerCarriedValue)
    -> WidgetWithBinding model msg innerCarriedValue
    -> WidgetWithBinding model msg outerCarriedValue
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
        mapParamsUp (\upInfo -> in2outUp upInfo) << mapParamsSub (\subInfo -> subInfo << andThen out2in)


stringOfIntWrapper :
    WidgetWithBinding model msg Int
    -> WidgetWithBinding model msg String
stringOfIntWrapper =
    makeBindingWrapper (alwaysOk toString) (ofResult << String.toInt)


intOfStringWrapper :
    WidgetWithBinding model msg String
    -> WidgetWithBinding model msg Int
intOfStringWrapper =
    makeBindingWrapper (ofResult << String.toInt) (alwaysOk toString)


plus2Wrapper :
    WidgetWithBinding model msg Int
    -> WidgetWithBinding model msg Int
plus2Wrapper =
    makeBindingWrapper (alwaysOk (\n -> n + 2)) (alwaysOk (\n -> n - 2))
