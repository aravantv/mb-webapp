module Binding exposing (..)

import Dict
import ListUtils exposing (..)
import LocalStorage
import MetaModel exposing (ClassRef, MetaModel, ModelType)
import Model exposing (Object)
import Widget exposing (ISelectable, Index, Path, makeTopWidget)


type alias BindingErr =
    { description : String }


type BindingResult serializedType
    = Ok serializedType
    | Err BindingErr
    | Irrelevant


type alias Binding msg serializedType =
    { get : Path -> Sub (BindingResult serializedType)
    , set : Path -> serializedType -> BindingResult (Cmd msg)
    }


type alias BindingTransformer msg ty1 ty2 =
    Binding msg ty1 -> Binding msg ty2


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


stringToIntBinding : BindingTransformer msg String Int
stringToIntBinding =
    mapBinding (ofResult << String.toInt) (alwaysOk toString)


intToStringBinding : BindingTransformer msg Int String
intToStringBinding =
    mapBinding (alwaysOk toString) (ofResult << String.toInt)


intToStringTransformer : BindingTransformer msg Int Int -> BindingTransformer msg String String
intToStringTransformer binding b =
    intToStringBinding (binding (stringToIntBinding b))


plus2Binding : BindingTransformer msg Int Int
plus2Binding =
    mapBinding (alwaysOk (\n -> n + 2)) (alwaysOk (\n -> n - 2))


type alias CollectionBinding msg relativePath =
    { itemAdded : Path -> Sub (BindingResult ( relativePath, Model.Model ))
    , itemRemoved : Path -> Sub (BindingResult relativePath)
    , addItem : Path -> relativePath -> Model.Model -> BindingResult (Cmd msg)
    , removeItem : Path -> relativePath -> BindingResult (Cmd msg)
    , askItemContent : Path -> relativePath -> Cmd msg
    , getAbsolutePath : Path -> relativePath -> Path
    }


type alias ListBinding msg =
    CollectionBinding msg Index


type alias ListBindingTransformer msg =
    ListBinding msg -> ListBinding msg


listBinding : MetaModel -> ModelType -> ListBinding msg
listBinding mm ty =
    { itemAdded =
        \p ->
            LocalStorage.itemAddedSub mm
                ty
                (\( path, maybeObj ) ->
                    case substract path p of
                        Just [ Widget.Index i ] ->
                            case maybeObj of
                                Result.Ok obj ->
                                    Ok ( i, obj )

                                Result.Err err ->
                                    Err { description = err }

                        _ ->
                            Irrelevant
                )
    , itemRemoved =
        \p ->
            LocalStorage.itemRemovedSub
                (\path ->
                    case substract path p of
                        Just [ Widget.Index i ] ->
                            Ok i

                        _ ->
                            Irrelevant
                )
    , addItem = \p i m -> Ok <| LocalStorage.addItemCmd (Index i :: p) m
    , removeItem = \p i -> Ok <| LocalStorage.removeItemCmd (Index i :: p)
    , askItemContent = \p i -> LocalStorage.askContentCmd (Index i :: p)
    , getAbsolutePath = \p i -> Index i :: p
    }


filterIntegerListBinding : ListBinding msg
filterIntegerListBinding =
    { itemAdded =
        \p ->
            LocalStorage.itemAddedSub Dict.empty
                MetaModel.Int
                (\( path, maybeObj ) ->
                    case substract path p of
                        Just [ Widget.Index i ] ->
                            case maybeObj of
                                Result.Ok obj ->
                                    Ok ( i, obj )

                                Result.Err err ->
                                    Err { description = err }

                        _ ->
                            Irrelevant
                )
    , itemRemoved =
        \p ->
            LocalStorage.itemRemovedSub
                (\path ->
                    case substract path p of
                        Just [ Widget.Index i ] ->
                            Ok i

                        _ ->
                            Irrelevant
                )
    , addItem = \p i m -> Ok <| LocalStorage.addItemCmd (Index i :: p) m
    , removeItem = \p i -> Ok <| LocalStorage.removeItemCmd (Index i :: p)
    , askItemContent = \p i -> LocalStorage.askContentCmd (Index i :: p)
    , getAbsolutePath = \p i -> Index i :: p
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
