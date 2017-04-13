module Binding exposing (..)

import ConstraintUtils exposing (UnfulfillmentInfo, Fixes(..))
import Data exposing (Data, Object)
import DataID exposing (DataID, getItemIdentifier, isItemOf)
import DataManager
import DataType exposing (ClassRef, DataType, DataTypeSet, FullDataType, emptyDataTypeSet, intDataType, stringDataType)
import LocalStorage
import Widget exposing (ISelectable, Index, makeTopWidget)


type BindingResult resType
    = Ok resType
    | Err UnfulfillmentInfo
    | Irrelevant


type alias ChildKey =
    String


type alias GenericBinding msg carriedValue =
    DataID
    -> { get : Sub (BindingResult carriedValue)
       , set : carriedValue -> BindingResult (Cmd msg)
       , datatype : FullDataType
       }


type alias Binding msg =
    GenericBinding msg Data.Data


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
    -> FullDataType
    -> GenericBinding msg t1
    -> GenericBinding msg t2
mapBinding fGet fSet fdt binding id =
    let
        concreteBinding =
            binding id
    in
        { get = andThenGet fGet concreteBinding.get
        , set = andThenSet fSet concreteBinding.set
        , datatype = fdt
        }


textBinding : GenericBinding msg String
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
    , datatype =
        { root = DataType.String
        , dataTypeSet = emptyDataTypeSet
        }
    }


stringToIntBinding : BindingTransformer msg String Int
stringToIntBinding =
    mapBinding (ofResult << String.toInt) (alwaysOk toString) intDataType


intToStringBinding : BindingTransformer msg Int String
intToStringBinding =
    mapBinding (alwaysOk toString) (ofResult << String.toInt) stringDataType


intToStringTransformer : BindingTransformer msg Int Int -> BindingTransformer msg String String
intToStringTransformer binding =
    intToStringBinding << binding << stringToIntBinding


plus2Binding : BindingTransformer msg Int Int
plus2Binding =
    mapBinding (alwaysOk (\n -> n + 2)) (alwaysOk (\n -> n - 2)) intDataType


type alias CollectionBinding msg relativePath =
    DataID
    -> { itemAdded : Sub (BindingResult ( relativePath, Data ))
       , itemRemoved : Sub (BindingResult relativePath)
       , addItem : relativePath -> Data -> BindingResult (Cmd msg)
       , removeItem : relativePath -> BindingResult (Cmd msg)
       , askItemContent : relativePath -> Cmd msg
       , getChildIdentifier : relativePath -> DataID
       }


type alias ListBinding msg =
    CollectionBinding msg Index


type alias ListBindingTransformer msg =
    ListBinding msg -> ListBinding msg


listBinding : DataTypeSet -> ListBinding msg
listBinding mm =
    \boundId ->
        { itemAdded =
            DataManager.itemAddedSub mm
                (\( id, maybeObj ) ->
                    case id |> isItemOf boundId of
                        Just i ->
                            case maybeObj of
                                Result.Ok obj ->
                                    Ok ( i, obj )

                                Result.Err err ->
                                    Err { unfulfillmentDescription = err, fixes = PossibleFixes [] }

                        _ ->
                            Irrelevant
                )
        , itemRemoved =
            DataManager.itemRemovedSub
                (\id ->
                    case id |> isItemOf boundId of
                        Just i ->
                            Ok i

                        _ ->
                            Irrelevant
                )
        , addItem = \i m -> Ok <| DataManager.addItemCmd (getItemIdentifier boundId i) m
        , removeItem = \i -> Ok <| DataManager.removeItemCmd (getItemIdentifier boundId i)
        , askItemContent = \i -> LocalStorage.askContentCmd (getItemIdentifier boundId i)
        , getChildIdentifier = \i -> getItemIdentifier boundId i
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
