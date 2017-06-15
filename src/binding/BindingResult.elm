module BindingResult exposing (..)

import ConstraintUtils exposing (Fixes(..), UnfulfillmentInfo, trivialUnfulfillmentInfo)


type BindingResult resType
    = Ok resType
    | Err UnfulfillmentInfo
    | Irrelevant


trivialErr : String -> BindingResult resType
trivialErr =
    Err << trivialUnfulfillmentInfo


filterIrrelevant : Bool -> BindingResult c -> BindingResult c
filterIrrelevant condition res =
    if condition then
        res
    else
        Irrelevant


alwaysOk : (t1 -> t2) -> (t1 -> BindingResult t2)
alwaysOk f x =
    Ok (f x)


ofMaybe : Maybe res -> String -> BindingResult res
ofMaybe maybe err =
    case maybe of
        Just v ->
            Ok v

        Nothing ->
            trivialErr err


ofResult : Result String res -> BindingResult res
ofResult res =
    case res of
        Result.Ok v ->
            Ok v

        Result.Err err ->
            trivialErr err


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
