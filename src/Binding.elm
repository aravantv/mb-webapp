module Binding exposing (..)

import ConstraintUtils exposing (Fixes(..), UnfulfillmentInfo)
import DataID exposing (DataID, getItemIdentifier, isItemOf, itemOf)
import DataManager
import IndexMapping exposing (IndexMapping)
import Widget exposing (ISelectable, Index, Widget, WidgetTransformer, mapParamsSub, mapParamsUp)


type BindingResult resType
    = Ok resType
    | Err UnfulfillmentInfo
    | Irrelevant


type alias ChildKey =
    String


type alias BindingSet carriedValue msg =
    carriedValue -> BindingResult (Cmd msg)


type alias BindingGet carriedValue =
    Sub (BindingResult carriedValue)


type alias GenericBinding msg carriedValue =
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
statelessWrapper fSet fGet =
    mapParamsUp (\set -> andThen set << fSet) << mapParamsSub (\get -> Sub.map (andThen fGet) get)


applyBinding :
    GenericBinding msg carriedValue
    -> Widget (BindingSet carriedValue msg) (BindingGet carriedValue) model msg
    -> Widget () () model msg
applyBinding b =
    mapParamsUp (\() -> b.set) << mapParamsSub (\() -> b.get)


textBinding : DataID -> GenericBinding msg String
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



{--
type alias GenericCollectionBinding msg relativePath carriedValue =
    DataID
    -> { itemAdded : Sub (BindingResult ( relativePath, carriedValue ))
       , itemRemoved : Sub (BindingResult relativePath)
       , addItem : relativePath -> carriedValue -> BindingResult (Cmd msg)
       , removeItem : relativePath -> BindingResult (Cmd msg)
       , askItemContent : relativePath -> Cmd msg
       , getChildIdentifier : relativePath -> DataID
       }


type alias CollectionBinding msg relativePath =
    GenericCollectionBinding msg relativePath Data


type alias ListBinding msg =
    CollectionBinding msg Index


type alias GenericListBinding msg carriedValue =
    GenericCollectionBinding msg Index carriedValue


type alias GenericListBinder model msg boundModel carriedValue =
    WidgetWrapper (GenericListBinding msg carriedValue) model msg boundModel


type alias ListBinder model msg boundModel =
    GenericListBinder model msg boundModel Data


type alias CollectionBindingTransformer msg relativePath carriedFrom carriedTo =
    GenericCollectionBinding msg relativePath carriedFrom -> GenericCollectionBinding msg relativePath carriedTo


type alias ListBindingTransformer msg carriedFrom carriedTo =
    CollectionBindingTransformer msg Index carriedFrom carriedTo


listBinding : DataTypeSet -> ListBinding msg
listBinding dts =
    \boundId ->
        { itemAdded =
            DataManager.itemAddedSub dts
                (\( id, maybeObj ) ->
                    case id |> itemOf boundId of
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
                    case id |> itemOf boundId of
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


stringToIntListBinding : BinderTransformer model msg boundModel String boundModel Int
stringToIntListBinding binder widgetBuilder =
    { itemAdded =
        Sub.map
            (\res ->
                case res of
                    Ok ( i, s ) ->
                        case String.toInt s of
                            Result.Ok n ->
                                ( Ok ( i, n ), (IndexMapping.insert idxMap i) )

                            Result.Err err ->
                                ( Err { unfulfillmentDescription = err, fixes = PossibleFixes [] }, ( newStrState, IndexMapping.insertButSkip idxMap i ) )

                    Err err ->
                        ( Err err, ( newStrState, idxMap ) )

                    Irrelevant ->
                        ( Irrelevant, ( newStrState, idxMap ) )
            )
            concreteBinding.itemAdded
    , itemRemoved =
        Sub.map
            (\( res, newStrState ) ->
                case res of
                    Ok i ->
                        ( res, ( newStrState, IndexMapping.remove idxMap i ) )

                    _ ->
                        ( res, ( newStrState, idxMap ) )
            )
            concreteBinding.itemRemoved
    , addItem = \i n -> concreteBinding.addItem i (toString n)
    , removeItem = concreteBinding.removeItem
    , askItemContent = concreteBinding.askItemContent
    , getChildIdentifier = concreteBinding.getChildIdentifier
    }


dataToStringListBinding : ListBindingTransformer msg Data String
dataToStringListBinding binding boundId ( strState, idxMap ) =
    let
        concreteBinding =
            binding boundId strState
    in
        { initState = ( concreteBinding.initState, IndexMapping.empty )
        , itemAdded =
            Sub.map
                (\( res, newStrState ) ->
                    case res of
                        Ok ( i, Data.String s ) ->
                            ( Ok ( i, s ), ( newStrState, IndexMapping.insert idxMap i ) )

                        Ok ( i, _ ) ->
                            ( Err { unfulfillmentDescription = "not a string", fixes = PossibleFixes [] }, ( newStrState, IndexMapping.insertButSkip idxMap i ) )

                        Err err ->
                            ( Err err, ( newStrState, idxMap ) )

                        Irrelevant ->
                            ( Irrelevant, ( newStrState, idxMap ) )
                )
                concreteBinding.itemAdded
        , itemRemoved =
            Sub.map
                (\( res, newStrState ) ->
                    case res of
                        Ok i ->
                            ( res, ( newStrState, IndexMapping.remove idxMap i ) )

                        _ ->
                            ( res, ( newStrState, idxMap ) )
                )
                concreteBinding.itemRemoved
        , addItem = \i s -> concreteBinding.addItem i (Data.String s)
        , removeItem = concreteBinding.removeItem
        , askItemContent = concreteBinding.askItemContent
        , getChildIdentifier = concreteBinding.getChildIdentifier
        }


intToDataListBinding : ListBindingTransformer msg Int Data
intToDataListBinding binding boundId state =
    let
        concreteBinding =
            binding boundId state
    in
        { initState = concreteBinding.initState
        , itemAdded = Sub.map (\( res, state ) -> ( map (\( i, n ) -> ( i, Data.Int n )) res, state )) concreteBinding.itemAdded
        , itemRemoved = concreteBinding.itemRemoved
        , addItem =
            \i d ->
                case d of
                    Data.Int n ->
                        concreteBinding.addItem i n

                    _ ->
                        Err { unfulfillmentDescription = "not an integer", fixes = PossibleFixes [] }
        , removeItem = concreteBinding.removeItem
        , askItemContent = concreteBinding.askItemContent
        , getChildIdentifier = concreteBinding.getChildIdentifier
        }
--}


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
