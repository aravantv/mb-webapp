module Binding exposing (..)

import LocalStorage
import MetaModel exposing (ClassRef, MetaModel, ModelElementIdentifier, ModelType, RootedMetaModel, emptyMetamodel, getItemIdentifier, intRootMetaModel, isItemOf, stringRootMetaModel)
import Model exposing (Object)
import Widget exposing (ISelectable, Index, makeTopWidget)


type alias BindingErr =
    { description : String }


type BindingResult resType
    = Ok resType
    | Err BindingErr
    | Irrelevant


type alias ChildKey =
    String


type alias GenericBinding msg carriedValue =
    ModelElementIdentifier
    -> { get : Sub (BindingResult carriedValue)
       , set : carriedValue -> BindingResult (Cmd msg)
       , metamodel : RootedMetaModel
       }


type alias Binding msg =
    GenericBinding msg Model.Model


type alias BindingTransformer msg carriedFrom carriedTo =
    GenericBinding msg carriedFrom -> GenericBinding msg carriedTo


andThenGet : (t1 -> BindingResult t2) -> Sub (BindingResult t1) -> Sub (BindingResult t2)
andThenGet f get =
    Sub.map (andThen f) get


andThenSet : (t1 -> BindingResult t2) -> (t2 -> BindingResult t3) -> (t1 -> BindingResult t3)
andThenSet f set val =
    andThen set (f val)


mapBinding :
    (t1 -> BindingResult t2)
    -> (t2 -> BindingResult t1)
    -> RootedMetaModel
    -> GenericBinding msg t1
    -> GenericBinding msg t2
mapBinding fGet fSet mm binding id =
    let
        concreteBinding =
            binding id
    in
        { get = andThenGet fGet concreteBinding.get
        , set = andThenSet fSet concreteBinding.set
        , metamodel = mm
        }


textBinding : GenericBinding msg String
textBinding boundId =
    { get =
        LocalStorage.getStringSub
            (\( id, s ) ->
                if id == boundId then
                    Ok s
                else
                    Irrelevant
            )
    , set = \s -> Ok <| LocalStorage.setStringCmd ( boundId, s )
    , metamodel =
        { root = MetaModel.String
        , metamodel = emptyMetamodel
        }
    }


stringToIntBinding : BindingTransformer msg String Int
stringToIntBinding =
    mapBinding (ofResult << String.toInt) (alwaysOk toString) intRootMetaModel


intToStringBinding : BindingTransformer msg Int String
intToStringBinding =
    mapBinding (alwaysOk toString) (ofResult << String.toInt) stringRootMetaModel


intToStringTransformer : BindingTransformer msg Int Int -> BindingTransformer msg String String
intToStringTransformer binding =
    intToStringBinding << binding << stringToIntBinding


plus2Binding : BindingTransformer msg Int Int
plus2Binding =
    mapBinding (alwaysOk (\n -> n + 2)) (alwaysOk (\n -> n - 2)) intRootMetaModel


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


listBinding : MetaModel -> ListBinding msg
listBinding mm =
    { itemAdded =
        \boundId ->
            LocalStorage.itemAddedSub mm
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
