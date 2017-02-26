module Binding exposing (..)

import ListUtils exposing (..)
import LocalStorage
import Widget exposing (ISelectable, Index, Path, makeTopWidget)


type alias BindingErr =
    { description : String }


type BindingResult res
    = Ok res
    | Err BindingErr
    | Irrelevant


type alias Binding msg serializedType =
    { get : Path -> Sub (BindingResult serializedType)
    , set : Path -> serializedType -> BindingResult (Cmd msg)
    }


type alias CollectionBinding msg err collectionPath =
    { itemAdded : Path -> Sub (Result err collectionPath)
    , itemRemoved : Path -> Sub (Result err collectionPath)
    , addItem : Path -> collectionPath -> Cmd msg
    , removeItem : Path -> collectionPath -> Cmd msg
    , askItemContent : Path -> collectionPath -> Cmd msg
    }


type alias ListBinding msg err =
    CollectionBinding msg err Index


textBinding : Binding msg String
textBinding =
    { get =
        \p ->
            LocalStorage.getStringSub
                (\( path, s ) ->
                    if path == p then
                        Ok s
                    else
                        Irrelevant
                )
    , set = \p s -> Ok <| LocalStorage.setStringCmd ( p, s )
    }


stringToIntBinding : Binding msg String -> Binding msg Int
stringToIntBinding binding =
    { get = \p -> Sub.map (andThen (ofResult << String.toInt)) (binding.get p)
    , set = \p v -> binding.set p (toString v)
    }


intToStringBinding : Binding msg Int -> Binding msg String
intToStringBinding binding =
    { get = \p -> Sub.map (\r -> map (\s -> toString s) r) (binding.get p)
    , set = \p v -> andThen (\n -> binding.set p n) (ofResult <| String.toInt v)
    }


plus2Binding : Binding msg Int
plus2Binding =
    let
        intBinding =
            stringToIntBinding textBinding
    in
        { get = \p -> Sub.map (\r -> map (\n -> n + 2) r) (intBinding.get p)
        , set = \p v -> intBinding.set p (v - 2)
        }


listBinding : ListBinding msg ()
listBinding =
    { itemAdded =
        \p ->
            LocalStorage.itemAddedSub
                (\path ->
                    case substract path p of
                        Just [ Widget.Index i ] ->
                            Result.Ok i

                        _ ->
                            Result.Err ()
                )
    , itemRemoved =
        \p ->
            LocalStorage.itemRemovedSub
                (\path ->
                    case substract path p of
                        Just [ Widget.Index i ] ->
                            Result.Ok i

                        _ ->
                            Result.Err ()
                )
    , addItem = \p i -> LocalStorage.addItemCmd (Index i :: p)
    , removeItem = \p i -> LocalStorage.removeItemCmd (Index i :: p)
    , askItemContent = \p i -> LocalStorage.askContentCmd (Index i :: p)
    }


ofResult : Result String res -> BindingResult res
ofResult res =
    case res of
        Result.Ok v ->
            Ok v

        Result.Err err ->
            Err { description = err }


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
