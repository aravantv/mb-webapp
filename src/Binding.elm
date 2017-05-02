module Binding exposing (..)

import ConstraintUtils exposing (Fixes(..), UnfulfillmentInfo)
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
            Err { unfulfillmentDescription = err, fixes = PossibleFixes [] }


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


type alias BindingSet carriedValue msg =
    carriedValue -> BindingResult (Cmd msg)


type alias BindingGet carriedValue =
    Sub (BindingResult carriedValue)


type alias Binding msg carriedValue =
    { get : BindingGet carriedValue
    , set : BindingSet carriedValue msg
    }


type alias BindingWrapper innerModel innerCarriedValue outerModel outerCarriedValue msg =
    Widget (BindingSet innerCarriedValue msg) (BindingGet innerCarriedValue) innerModel msg
    -> Widget (BindingSet outerCarriedValue msg) (BindingGet outerCarriedValue) outerModel msg


statelessWrapper :
    (innerCarriedValue -> BindingResult outerCarriedValue)
    -> (outerCarriedValue -> BindingResult innerCarriedValue)
    -> BindingWrapper model innerCarriedValue model outerCarriedValue msg
statelessWrapper in2out out2in =
    mapParamsUp (\set -> andThen set << in2out) << mapParamsSub (\get -> Sub.map (andThen out2in) get)


applyBinding :
    Widget (BindingSet carriedValue msg) (BindingGet carriedValue) model msg
    -> Binding msg carriedValue
    -> Widget () () model msg
applyBinding w b =
    mapParamsUp (\() -> b.set) (mapParamsSub (\() -> b.get) w)


textBinding : DataID -> Binding msg String
textBinding boundId =
    { get =
        DataManager.getStringSub
            (\( id, s ) ->
                if id == boundId then
                    Ok s
                else
                    Irrelevant
            )
    , set = \s -> Ok <| DataManager.setStringCmd ( boundId, s )
    }


stringOfIntWrapper : BindingWrapper model Int model String msg
stringOfIntWrapper =
    statelessWrapper (alwaysOk toString) (ofResult << String.toInt)


intOfStringWrapper : BindingWrapper model String model Int msg
intOfStringWrapper =
    statelessWrapper (ofResult << String.toInt) (alwaysOk toString)


plus2Wrapper : BindingWrapper model Int model Int msg
plus2Wrapper =
    statelessWrapper (alwaysOk (\n -> n + 2)) (alwaysOk (\n -> n - 2))
