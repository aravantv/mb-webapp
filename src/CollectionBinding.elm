module CollectionBinding exposing (..)

import Binding exposing (BindingResult)
import ConstraintUtils exposing (Fixes(..), UnfulfillmentInfo)
import Data
import DataID exposing (DataID, getItemIdentifier, itemOf)
import DataManager
import DataType exposing (DataTypeSet)
import Html
import IndexMapping
import Widget exposing (Widget, mapParamsSub, mapParamsUp, TopWidget, cmdOfMsg)


type CollectionBindingUpInfo collectionPath carriedValue
    = AddItem (BindingResult ( collectionPath, carriedValue ))
    | RemoveItem (BindingResult collectionPath)


mapUpInfo :
    (carriedFromValue -> carriedToValue)
    -> CollectionBindingUpInfo collectionPath carriedFromValue
    -> CollectionBindingUpInfo collectionPath carriedToValue
mapUpInfo f i =
    case i of
        AddItem res ->
            AddItem (Binding.map (\( i, v ) -> ( i, f v )) res)

        RemoveItem res ->
            RemoveItem res


type alias CollectionBindingSubInfo collectionPath carriedValue msg =
    { itemAdded : BindingResult ( collectionPath, carriedValue ) -> msg
    , itemRemoved : BindingResult collectionPath -> msg
    }


type alias WidgetWithCollectionBinding collectionPath model msg carriedValue =
    Widget (CollectionBindingUpInfo collectionPath carriedValue) (CollectionBindingSubInfo collectionPath carriedValue msg) model msg


type alias CollectionBindingAdapter collectionPath innerModel innerCarriedValue innerMsg outerModel outerCarriedValue outerMsg =
    WidgetWithCollectionBinding collectionPath innerModel innerMsg innerCarriedValue
    -> WidgetWithCollectionBinding collectionPath outerModel outerMsg outerCarriedValue


type alias CollectionBindingWrapper collectionPath innerModel innerMsg carriedValue outerModel outerMsg =
    WidgetWithCollectionBinding collectionPath innerModel innerMsg carriedValue
    -> Widget () () outerModel outerMsg


mapItemAdded :
    (fromValue -> BindingResult toValue)
    -> BindingResult ( collectionPath, fromValue )
    -> BindingResult ( collectionPath, toValue )
mapItemAdded out2in =
    Binding.andThen (\( i, s ) -> Binding.map (\n -> ( i, n )) (out2in s))


applyBinding :
    WidgetWithCollectionBinding collectionPath model msg carriedValue
    -> { addItem : collectionPath -> carriedValue -> Cmd msg
       , removeItem : collectionPath -> Cmd msg
       , itemAdded : (BindingResult ( collectionPath, carriedValue ) -> msg) -> Sub msg
       , itemRemoved : (BindingResult collectionPath -> msg) -> Sub msg
       }
    -> Widget () () model msg
applyBinding w b id =
    let
        cw =
            w id
    in
        { initModel = cw.initModel
        , initMsg = \d -> cw.initMsg d
        , update =
            \msg model ->
                let
                    ( newModel, cmd, upInfo ) =
                        cw.update msg model

                    newCmd =
                        case upInfo of
                            AddItem (Binding.Ok ( idx, val )) ->
                                Cmd.batch [ cmd, b.addItem idx val ]

                            AddItem _ ->
                                cmd

                            RemoveItem (Binding.Ok idx) ->
                                Cmd.batch [ cmd, b.removeItem idx ]

                            RemoveItem _ ->
                                cmd
                in
                    ( newModel, newCmd, () )
        , subscriptions =
            \model ->
                let
                    ( sub, mapper ) =
                        cw.subscriptions model
                in
                    ( Sub.batch (sub :: [ b.itemAdded mapper.itemAdded, b.itemRemoved mapper.itemRemoved ]), () )
        , view = cw.view
        }


type alias Index =
    Int


listBinding :
    DataTypeSet
    -> DataID
    -> { addItem : Index -> Data.Data -> Cmd msg
       , removeItem : Index -> Cmd msg
       , itemAdded : (BindingResult ( Index, Data.Data ) -> msg) -> Sub msg
       , itemRemoved : (BindingResult Index -> msg) -> Sub msg
       }
listBinding dts boundId =
    { itemAdded =
        \f ->
            DataManager.itemAddedSub dts
                (\( id, maybeObj ) ->
                    f
                        (case id |> itemOf boundId of
                            Just i ->
                                case maybeObj of
                                    Result.Ok obj ->
                                        Binding.Ok ( i, obj )

                                    Result.Err err ->
                                        Binding.Err { unfulfillmentDescription = err, fixes = PossibleFixes [] }

                            _ ->
                                Binding.Irrelevant
                        )
                )
    , itemRemoved =
        \f ->
            DataManager.itemRemovedSub
                (\id ->
                    f
                        (case id |> itemOf boundId of
                            Just i ->
                                Binding.Ok i

                            _ ->
                                Binding.Irrelevant
                        )
                )
    , addItem = \i m -> DataManager.addItemCmd (getItemIdentifier boundId i) m
    , removeItem = \i -> DataManager.removeItemCmd (getItemIdentifier boundId i)
    }


type CollectionBindingMsg collectionPath
    = ItemAdded collectionPath
    | ItemAddedButSkipped collectionPath
    | ItemRemoved collectionPath


stringOfIntBindingWrapper :
    WidgetWithCollectionBinding collectionPath innerModel innerMsg Int
    -> WidgetWithCollectionBinding collectionPath ( innerModel, IndexMapping.IndexMapping ) ( innerMsg, Maybe (CollectionBindingMsg collectionPath) ) String
stringOfIntBindingWrapper w id =
    let
        cw =
            w id
    in
        { initModel = ( cw.initModel, IndexMapping.empty )
        , initMsg = \d -> ( cw.initMsg d, Nothing )
        , update =
            \( msg, _ ) ( model, mapping ) ->
                let
                    ( newModel, cmd, info ) =
                        cw.update msg model
                in
                    ( ( newModel, mapping ), Cmd.map (\m -> ( m, Nothing )) cmd, mapUpInfo toString info )
        , subscriptions =
            \( model, _ ) ->
                let
                    ( sub, info ) =
                        cw.subscriptions model

                    newInfo =
                        { itemAdded =
                            \res ->
                                case res of
                                    Binding.Ok ( i, s ) ->
                                        let
                                            intRes =
                                                String.toInt s
                                        in
                                            case intRes of
                                                Ok n ->
                                                    ( info.itemAdded (Binding.Ok ( i, n ))
                                                    , Nothing
                                                    )

                                                Err err ->
                                                    ( info.itemAdded (Binding.Err { unfulfillmentDescription = err, fixes = PossibleFixes [] })
                                                    , Nothing
                                                    )

                                    Binding.Err err ->
                                        ( info.itemAdded (Binding.Err err)
                                        , Nothing
                                        )

                                    Binding.Irrelevant ->
                                        ( info.itemAdded Binding.Irrelevant
                                        , Nothing
                                        )
                        , itemRemoved =
                            \res -> ( info.itemRemoved res, Nothing )
                        }
                in
                    ( Sub.map (\m -> ( m, Nothing )) sub, newInfo )
        , view = \( model, _ ) -> Html.map (\m -> ( m, Nothing )) (cw.view model)
        }



{--
intOfStringWrapper :
    CollectionBindingWrapper collectionPath model msg String
    -> CollectionBindingWrapper collectionPath model msg Int
intOfStringWrapper wrapper w id =
    let
        cw =
            w id
    in
        wrapper
            { initModel = ( cw.initModel, IndexMapping.empty )
            , initMsg = cw.initMsg
            , update =
                \( wrappedMsg, bindingMsg ) ( model, idxMap ) paramsUps ->
                    let
                        ( newModel, cmd ) =
                            cw.update wrappedMsg
                                model
                                { addItem =
                                    \i n -> paramsUps.addItem i (toString n)
                                , removeItem = paramsUps.removeItem
                                }

                        newIdxMap =
                            case bindingMsg of
                                Nothing ->
                                    idxMap

                                Just (ItemAdded idx) ->
                                    IndexMapping.insert idxMap idx

                                Just (ItemAddedButSkipped idx) ->
                                    IndexMapping.insertButSkip idxMap idx

                                Just (ItemRemoved idx) ->
                                    IndexMapping.remove idxMap idx
                    in
                        ( ( newModel, newIdxMap ), cmd )
            , subscriptions =
                \( model, idxMap ) paramsSubs ->
                    cw.subscriptions model
                        { itemAdded = Sub.map (Binding.andThen (\( i, s ) -> Binding.map (\n -> ( i, n )) <| Binding.ofResult (String.toInt s))) paramsSubs.itemAdded
                        , itemRemoved = paramsSubs.itemRemoved
                        }
            , view = cw.view
            }
            id
--}
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
