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


andThenGet : (t1 -> BindingResult t2) -> (Path -> Sub (BindingResult t1)) -> (Path -> Sub (BindingResult t2))
andThenGet f get p =
    Sub.map (andThen f) (get p)


andThenSet : (t1 -> BindingResult t2) -> (Path -> t2 -> BindingResult (Cmd msg)) -> (Path -> t1 -> BindingResult (Cmd msg))
andThenSet f set p val =
    andThen (\n -> set p n) (f val)


mapBinding : (t1 -> BindingResult t2) -> (t2 -> BindingResult t1) -> Binding msg t1 -> Binding msg t2
mapBinding fGet fSet binding =
    { get = andThenGet fGet binding.get
    , set = andThenSet fSet binding.set
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
stringToIntBinding =
    mapBinding (ofResult << String.toInt) (alwaysOk toString)


intToStringBinding : Binding msg Int -> Binding msg String
intToStringBinding =
    mapBinding (alwaysOk toString) (ofResult << String.toInt)


plus2Binding : Binding msg Int
plus2Binding =
    mapBinding (alwaysOk (\n -> n + 2)) (alwaysOk (\n -> n - 2)) (stringToIntBinding textBinding)


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


alwaysOk : (t1 -> t2) -> (t1 -> BindingResult t2)
alwaysOk f x =
    Ok (f x)


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
