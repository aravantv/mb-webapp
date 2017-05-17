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


type alias BindingUpInfo carriedValue =
    BindingResult carriedValue


type alias BindingSubInfo carriedValue msg =
    BindingResult carriedValue -> msg


type alias WidgetWithBinding model msg carriedValue =
    Widget (BindingUpInfo carriedValue) (BindingSubInfo carriedValue msg) model msg


type alias BindingWrapper innerModel innerCarriedValue outerModel outerCarriedValue msg =
    WidgetWithBinding innerModel msg innerCarriedValue
    -> WidgetWithBinding outerModel msg outerCarriedValue


statelessWrapper :
    (innerCarriedValue -> BindingResult outerCarriedValue)
    -> (outerCarriedValue -> BindingResult innerCarriedValue)
    -> BindingWrapper model innerCarriedValue model outerCarriedValue msg
statelessWrapper in2out out2in =
    mapParamsUp (\set -> andThen set << in2out) << mapParamsSub (\get -> Sub.map (andThen out2in) get)


type alias Binding msg carriedValue =
    { set : BindingResult carriedValue -> Cmd msg
    , get : (BindingResult carriedValue -> msg) -> Sub msg
    }


applyBinding :
    WidgetWithBinding model msg carriedValue
    -> Binding msg carriedValue
    -> Widget () () model msg
applyBinding w b id =
    let
        cw =
            w id
    in
        { initModel = cw.initModel
        , initMsg = cw.initMsg
        , update =
            \msg model ->
                let
                    ( newModel, cmd, upInfo ) =
                        cw.update msg model
                in
                    ( newModel, Cmd.batch [ cmd, b.set upInfo ], () )
        , subscriptions =
            \model ->
                let
                    ( sub, mapper ) =
                        cw.subscriptions model
                in
                    ( Sub.batch [ sub, b.get mapper ], () )
        , view = cw.view
        }


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
