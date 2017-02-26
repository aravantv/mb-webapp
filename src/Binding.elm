module Binding exposing (..)

import ListUtils exposing (..)
import LocalStorage
import Widget exposing (Path, ISelectable, Index, makeTopWidget)


type alias Binding msg serializedType err =
    { get : Path -> Sub (Result err serializedType)
    , set : Path -> serializedType -> Cmd msg
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


textBinding : Binding msg String String
textBinding =
    { get =
        \p ->
            LocalStorage.getStringSub
                (\( path, s ) ->
                    if path == p then
                        Result.Ok s
                    else
                        Result.Err "Value received irrelevant for this binding."
                )
    , set = \p s -> LocalStorage.setStringCmd ( p, s )
    }


stringToIntBinding : Binding msg String String -> Binding msg Int String
stringToIntBinding binding =
    { get = \p -> Sub.map (Result.andThen String.toInt) (binding.get p)
    , set = \p v -> binding.set p (toString v)
    }


intToStringBinding : Binding msg Int String -> Binding msg String String
intToStringBinding binding =
    { get = \p -> Sub.map (\r -> Result.map (\s -> toString s) r) (binding.get p)
    , set =
        \p v ->
            case String.toInt v of
                Err _ ->
                    Cmd.none

                Ok n ->
                    binding.set p n
    }


plus2Binding : Binding msg Int String
plus2Binding =
    let
        intBinding =
            stringToIntBinding textBinding
    in
        { get = \p -> Sub.map (\r -> Result.map (\n -> n + 2) r) (intBinding.get p)
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
