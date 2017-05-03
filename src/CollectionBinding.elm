module CollectionBinding exposing (..)

import Binding exposing (BindingResult)
import ConstraintUtils exposing (Fixes(..), UnfulfillmentInfo)
import Widget exposing (Widget, mapParamsSub, mapParamsUp)


type alias BindingAddItem collectionPath carriedValue msg =
    collectionPath -> carriedValue -> BindingResult (Cmd msg)


type alias BindingRemoveItem collectionPath msg =
    collectionPath -> BindingResult (Cmd msg)


type alias CollectionBindingUpdate collectionPath carriedValue msg =
    { addItem : BindingAddItem collectionPath carriedValue msg
    , removeItem : BindingRemoveItem collectionPath msg
    }


type alias BindingItemAdded collectionPath carriedValue =
    Sub (BindingResult ( collectionPath, carriedValue ))


type alias BindingItemRemoved collectionPath =
    Sub (BindingResult collectionPath)


type alias CollectionBindingSubscriptions collectionPath carriedValue =
    { itemAdded : BindingItemAdded collectionPath carriedValue
    , itemRemoved : BindingItemRemoved collectionPath
    }


type alias CollectionBinding collectionPath msg carriedValue =
    { addItem : BindingAddItem collectionPath carriedValue msg
    , removeItem : BindingRemoveItem collectionPath msg
    , itemAdded : BindingItemAdded collectionPath carriedValue
    , itemRemoved : BindingItemRemoved collectionPath
    }


type alias WidgetWithCollectionBinding collectionPath model msg carriedValue =
    Widget (CollectionBindingUpdate collectionPath carriedValue msg) (CollectionBindingSubscriptions collectionPath carriedValue) model msg


type alias CollectionBindingWrapper collectionPath innerModel innerCarriedValue outerModel outerCarriedValue msg =
    WidgetWithCollectionBinding collectionPath innerModel msg innerCarriedValue
    -> WidgetWithCollectionBinding collectionPath outerModel msg outerCarriedValue


statelessWrapper :
    (innerCarriedValue -> BindingResult outerCarriedValue)
    -> (outerCarriedValue -> BindingResult innerCarriedValue)
    -> CollectionBindingWrapper collectionPath model innerCarriedValue model outerCarriedValue msg
statelessWrapper in2out out2in =
    mapParamsUp
        (\up ->
            { addItem = \path -> andThen (up.addItem path) << in2out
            , removeItem = up.removeItem
            }
        )
        << mapParamsSub
            (\sub ->
                { itemAdded =
                    Sub.map
                        (andThen (\( path, value ) -> Binding.map (\x -> ( path, x )) (out2in value)))
                        sub.itemAdded
                , itemRemoved = sub.itemRemoved
                }
            )


applyBinding :
    WidgetWithCollectionBinding collectionPath model msg carriedValue
    -> CollectionBinding collectionPath msg carriedValue
    -> Widget () () model msg
applyBinding w b =
    mapParamsUp (\() -> { addItem = b.addItem, removeItem = b.removeItem })
        (mapParamsSub (\() -> { itemAdded = b.itemAdded, itemRemoved = b.itemRemoved }) w)



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
