module Binding exposing (..)

import LocalStorage
import MetaModel exposing (ClassRef, MetaModel, ModelElementIdentifier, ModelType, getItemIdentifier, isItemOf)
import Model exposing (Object)
import Widget exposing (ISelectable, Index, makeTopWidget)


type alias BindingErr =
    { description : String }


type BindingResult serializedType
    = Ok serializedType
    | Err BindingErr
    | Irrelevant


type alias Binding msg serializedType =
    { get : ModelElementIdentifier -> Sub (BindingResult serializedType)
    , set : ModelElementIdentifier -> serializedType -> BindingResult (Cmd msg)
    }


type alias BindingTransformer msg ty1 ty2 =
    Binding msg ty1 -> Binding msg ty2


andThenGet : (t1 -> BindingResult t2) -> (ModelElementIdentifier -> Sub (BindingResult t1)) -> (ModelElementIdentifier -> Sub (BindingResult t2))
andThenGet f get p =
    Sub.map (andThen f) (get p)


andThenSet : (t1 -> BindingResult t2) -> (ModelElementIdentifier -> t2 -> BindingResult (Cmd msg)) -> (ModelElementIdentifier -> t1 -> BindingResult (Cmd msg))
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
        \boundId ->
            LocalStorage.getStringSub
                (\( id, s ) ->
                    if id == boundId then
                        Ok s
                    else
                        Irrelevant
                )
    , set = \boundId s -> Ok <| LocalStorage.setStringCmd ( boundId, s )
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
    { itemAdded : ModelElementIdentifier -> Sub (BindingResult ( relativePath, Model.Model ))
    , itemRemoved : ModelElementIdentifier -> Sub (BindingResult relativePath)
    , addItem : ModelElementIdentifier -> relativePath -> Model.Model -> BindingResult (Cmd msg)
    , removeItem : ModelElementIdentifier -> relativePath -> BindingResult (Cmd msg)
    , askItemContent : ModelElementIdentifier -> relativePath -> Cmd msg
    , getChildIdentifier : ModelElementIdentifier -> relativePath -> ModelElementIdentifier
    }


type alias ListBinding msg =
    CollectionBinding msg Index


type alias ListBindingTransformer msg =
    ListBinding msg -> ListBinding msg


listBinding : MetaModel -> ModelType -> ListBinding msg
listBinding mm ty =
    { itemAdded =
        \boundId ->
            LocalStorage.itemAddedSub mm
                ty
                (\( id, maybeObj ) ->
                    case id |> isItemOf boundId of
                        Just i ->
                            case maybeObj of
                                Result.Ok obj ->
                                    Ok ( i, obj )

                                Result.Err err ->
                                    Err { description = err }

                        _ ->
                            Irrelevant
                )
    , itemRemoved =
        \boundId ->
            LocalStorage.itemRemovedSub
                (\id ->
                    case id |> isItemOf boundId of
                        Just i ->
                            Ok i

                        _ ->
                            Irrelevant
                )
    , addItem = \boundId i m -> Ok <| LocalStorage.addItemCmd (getItemIdentifier boundId i) m
    , removeItem = \boundId i -> Ok <| LocalStorage.removeItemCmd (getItemIdentifier boundId i)
    , askItemContent = \boundId i -> LocalStorage.askContentCmd (getItemIdentifier boundId i)
    , getChildIdentifier = \id i -> getItemIdentifier id i
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
