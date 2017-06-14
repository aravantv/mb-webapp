module CollectionBinding exposing (..)

import Utils exposing (Index)
import Binding exposing (BindingResult, alwaysOk, trivialErr)
import ConstraintUtils exposing (Fixes(..), UnfulfillmentInfo)
import Data exposing (AttributeValue(..), Data)
import DataManager exposing (DataID)
import Html exposing (sub)
import IndexMapping exposing (IndexMapping)
import Widget exposing (TopWidget, Widget, cmdOf, cmdOfMsg, mapParamsSub, mapParamsUp, modelOf)


mapIndexValueResult :
    (fromValue -> BindingResult toValue)
    -> BindingResult ( collectionPath, fromValue )
    -> BindingResult ( collectionPath, toValue )
mapIndexValueResult out2in =
    Binding.andThen (\( i, v ) -> Binding.map (\n -> ( i, n )) (out2in v))


type CollectionBindingSymbolicCmd collectionPath carriedValue
    = AddItem (BindingResult ( collectionPath, carriedValue ))
    | RemoveItem (BindingResult collectionPath)
    | ModifyItem (BindingResult ( collectionPath, carriedValue ))
    | DoNothing


mapCollectionPath :
    (collectionPath -> Maybe collectionPath)
    -> CollectionBindingSymbolicCmd collectionPath carriedValue
    -> CollectionBindingSymbolicCmd collectionPath carriedValue
mapCollectionPath f info =
    let
        errMsg =
            "mapCollectionPath: collectionPath could not be transformed"
    in
        case info of
            AddItem (Binding.Ok ( p, v )) ->
                AddItem <|
                    case f p of
                        Just newP ->
                            Binding.Ok ( newP, v )

                        Nothing ->
                            trivialErr errMsg

            RemoveItem (Binding.Ok p) ->
                RemoveItem <|
                    case f p of
                        Just newP ->
                            Binding.Ok newP

                        Nothing ->
                            trivialErr errMsg

            x ->
                x


doNothing : a -> ( a, Cmd msg, CollectionBindingSymbolicCmd collectionPath carriedValue )
doNothing x =
    ( x, Cmd.none, DoNothing )


mapSymbolicCmd :
    (carriedFromValue -> BindingResult carriedToValue)
    -> CollectionBindingSymbolicCmd collectionPath carriedFromValue
    -> CollectionBindingSymbolicCmd collectionPath carriedToValue
mapSymbolicCmd f i =
    case i of
        AddItem res ->
            AddItem (mapIndexValueResult f res)

        ModifyItem res ->
            ModifyItem (mapIndexValueResult f res)

        RemoveItem res ->
            RemoveItem res

        DoNothing ->
            DoNothing


type alias CollectionBindingSymbolicSub collectionPath carriedValue msg =
    { itemAdded : BindingResult ( collectionPath, carriedValue ) -> msg
    , itemRemoved : BindingResult collectionPath -> msg
    , itemModified : BindingResult ( collectionPath, carriedValue ) -> msg
    , getFullList : BindingResult (List carriedValue) -> msg
    }


type alias BoundCollectionWidget collectionPath model msg carriedValue =
    Widget (CollectionBindingSymbolicCmd collectionPath carriedValue) (CollectionBindingSymbolicSub collectionPath carriedValue msg) model msg


type alias BoundListWidget model msg carriedValue =
    BoundCollectionWidget Index model msg carriedValue


type alias CollectionBinding collectionPath msg carriedValue =
    { addItem : collectionPath -> carriedValue -> Cmd msg
    , removeItem : collectionPath -> Cmd msg
    , modifyItem : collectionPath -> carriedValue -> Cmd msg
    , itemAdded : (BindingResult ( collectionPath, carriedValue ) -> msg) -> Sub msg
    , itemModified : (BindingResult ( collectionPath, carriedValue ) -> msg) -> Sub msg
    , itemRemoved : (BindingResult collectionPath -> msg) -> Sub msg
    , getFullList : (BindingResult (List carriedValue) -> msg) -> Sub msg
    , ask : Cmd msg
    }


applyListBinding :
    CollectionBinding Index msg Data
    -> BoundCollectionWidget Index model msg Data
    -> Widget () () model msg
applyListBinding b w =
    { init = ( modelOf w.init, Cmd.batch [ cmdOf w.init, b.ask ] )
    , update =
        \msg model ->
            let
                ( newModel, cmd, symbolicCmd ) =
                    w.update msg model

                maybeConcreteCmd =
                    case symbolicCmd of
                        AddItem (Binding.Ok ( idx, val )) ->
                            Just (b.addItem idx val)

                        ModifyItem (Binding.Ok ( idx, val )) ->
                            Just (b.modifyItem idx val)

                        RemoveItem (Binding.Ok idx) ->
                            Just (b.removeItem idx)

                        _ ->
                            Nothing

                newCmd =
                    case maybeConcreteCmd of
                        Just concreteCmd ->
                            Cmd.batch [ cmd, concreteCmd ]

                        Nothing ->
                            cmd
            in
                ( newModel, newCmd, () )
    , subscriptions =
        \model ->
            let
                ( sub, symbolicSub ) =
                    w.subscriptions model

                concreteSubs =
                    [ b.itemAdded symbolicSub.itemAdded
                    , b.itemRemoved symbolicSub.itemRemoved
                    , b.getFullList symbolicSub.getFullList
                    ]
            in
                ( Sub.batch (sub :: concreteSubs), () )
    , view = w.view
    }


listBinding : DataID -> CollectionBinding Index msg Data.Data
listBinding boundId =
    { itemAdded =
        \buildMsg ->
            DataManager.itemAddedSub
                (\collectionID i maybeObj ->
                    buildMsg
                        (if collectionID == boundId then
                            case maybeObj of
                                Result.Ok obj ->
                                    Binding.Ok ( i, obj )

                                Result.Err err ->
                                    Binding.Err { unfulfillmentDescription = err, fixes = PossibleFixes [] }
                         else
                            Binding.Irrelevant
                        )
                )
    , itemModified =
        \buildMsg ->
            DataManager.getDataSub
                (\id _ maybeObj path ->
                    buildMsg
                        (if id == boundId then
                            case maybeObj of
                                Result.Ok (SingleData (Just obj)) ->
                                    case path of
                                        [ s ] ->
                                            case String.toInt s of
                                                Ok i ->
                                                    Binding.Ok ( i, obj )

                                                Err _ ->
                                                    trivialErr "Path is not an index: please report"

                                        _ ->
                                            Binding.Irrelevant

                                Result.Ok _ ->
                                    trivialErr "A list was expected but the storage contains something else: please report"

                                Result.Err err ->
                                    trivialErr err
                         else
                            Binding.Irrelevant
                        )
                )
    , itemRemoved =
        \buildMsg ->
            DataManager.itemRemovedSub
                (\collectionID i ->
                    buildMsg
                        (if collectionID == boundId then
                            Binding.Ok i
                         else
                            Binding.Irrelevant
                        )
                )
    , addItem = \i m -> DataManager.addItemCmd boundId i m
    , modifyItem = \i m -> DataManager.modifyItemCmd boundId i m
    , removeItem = \i -> DataManager.removeItemCmd boundId i
    , getFullList =
        \f ->
            DataManager.getDataSub
                (\id _ maybeObj path ->
                    f <|
                        if id == boundId && path == [] then
                            case maybeObj of
                                Result.Ok (MultipleData datas) ->
                                    Binding.Ok datas

                                Result.Ok _ ->
                                    trivialErr "A list was expected but the storage contains something else: please report"

                                Result.Err err ->
                                    trivialErr err
                        else
                            Binding.Irrelevant
                )
    , ask = DataManager.askDataCmd boundId
    }


type alias WrappedBoundListWidget model msg carriedValue =
    BoundListWidget ( model, IndexMapping ) (IndexMapping -> ( msg, IndexMapping )) carriedValue


makeListBindingWrapper :
    (inCarriedValue -> BindingResult outCarriedValue)
    -> (outCarriedValue -> BindingResult inCarriedValue)
    -> BoundListWidget innerModel innerMsg inCarriedValue
    -> WrappedBoundListWidget innerModel innerMsg outCarriedValue
makeListBindingWrapper in2out out2in w =
    let
        trivialMsg m =
            \idxMap -> ( m, idxMap )

        msgOfItemValue k ( i, s ) =
            case out2in s of
                Binding.Ok n ->
                    \idxMap ->
                        let
                            newIdxMap =
                                IndexMapping.insert idxMap i

                            res =
                                case IndexMapping.get newIdxMap i of
                                    Just j ->
                                        Binding.Ok ( j, n )

                                    Nothing ->
                                        trivialErr "Index not found - please report"
                        in
                            ( k res, newIdxMap )

                Binding.Err err ->
                    \idxMap ->
                        ( k (Binding.Err err)
                        , IndexMapping.insertButSkip idxMap i
                        )

                Binding.Irrelevant ->
                    \idxMap ->
                        ( k Binding.Irrelevant
                        , IndexMapping.insertButSkip idxMap i
                        )

        msgOfBindingRes k okCase res =
            case res of
                Binding.Ok v ->
                    okCase k v

                Binding.Err err ->
                    trivialMsg (k (Binding.Err err))

                Binding.Irrelevant ->
                    trivialMsg (k Binding.Irrelevant)
    in
        { init = ( ( modelOf w.init, IndexMapping.empty ), Cmd.map trivialMsg (cmdOf w.init) )
        , update =
            \msg ( model, idxMap ) ->
                let
                    ( subMsg, newIdxMap ) =
                        msg idxMap

                    ( newModel, cmd, info ) =
                        w.update subMsg model

                    newInfo =
                        mapCollectionPath (\i -> IndexMapping.retrieve newIdxMap i) info
                in
                    ( ( newModel, newIdxMap ), Cmd.map trivialMsg cmd, mapSymbolicCmd in2out newInfo )
        , subscriptions =
            \( model, _ ) ->
                let
                    ( sub, info ) =
                        w.subscriptions model

                    newInfo =
                        { itemAdded = msgOfBindingRes info.itemAdded msgOfItemValue
                        , itemModified = msgOfBindingRes info.itemModified msgOfItemValue
                        , itemRemoved =
                            \res idxMap ->
                                case res of
                                    Binding.Ok i ->
                                        let
                                            newIdxMap =
                                                IndexMapping.remove idxMap i

                                            newRes =
                                                case IndexMapping.get idxMap i of
                                                    Just j ->
                                                        Binding.Ok j

                                                    Nothing ->
                                                        trivialErr "Index not found - please report"
                                        in
                                            ( info.itemRemoved newRes, newIdxMap )

                                    _ ->
                                        ( info.itemRemoved res, idxMap )
                        , getFullList =
                            msgOfBindingRes info.getFullList <|
                                \mapper xs ->
                                    let
                                        ( newRes, idxMap, _ ) =
                                            List.foldr
                                                (\outValue ( inValues, idxMap, i ) ->
                                                    case out2in outValue of
                                                        Binding.Ok inValue ->
                                                            ( inValue :: inValues, IndexMapping.insert idxMap i, i + 1 )

                                                        _ ->
                                                            ( inValues, IndexMapping.insertButSkip idxMap i, i + 1 )
                                                )
                                                ( [], IndexMapping.empty, 0 )
                                                xs
                                    in
                                        \_ -> ( mapper (Binding.Ok newRes), idxMap )
                        }
                in
                    ( Sub.map trivialMsg sub, newInfo )
        , view = \( model, _ ) -> Html.map trivialMsg (w.view model)
        }


stringOfIntBindingWrapper :
    BoundListWidget innerModel innerMsg Int
    -> WrappedBoundListWidget innerModel innerMsg String
stringOfIntBindingWrapper =
    makeListBindingWrapper (Binding.alwaysOk toString) (Binding.ofResult << String.toInt)


intOfStringBindingWrapper :
    BoundListWidget innerModel innerMsg String
    -> WrappedBoundListWidget innerModel innerMsg Int
intOfStringBindingWrapper =
    makeListBindingWrapper (Binding.ofResult << String.toInt) (Binding.alwaysOk toString)


stringOfData : Data.Data -> BindingResult String
stringOfData d =
    case d of
        Data.String s ->
            Binding.Ok s

        _ ->
            trivialErr "Not a string"


dataOfStringBindingWrapper :
    BoundListWidget innerModel innerMsg String
    -> WrappedBoundListWidget innerModel innerMsg Data.Data
dataOfStringBindingWrapper =
    makeListBindingWrapper (Binding.alwaysOk Data.String) stringOfData


stringOfDataBindingWrapper :
    BoundListWidget innerModel innerMsg Data.Data
    -> WrappedBoundListWidget innerModel innerMsg String
stringOfDataBindingWrapper =
    makeListBindingWrapper stringOfData (Binding.alwaysOk Data.String)


intOfData : Data.Data -> BindingResult Int
intOfData d =
    case d of
        Data.Int n ->
            Binding.Ok n

        _ ->
            trivialErr "Not an integer"


dataOfIntBindingWrapper :
    BoundListWidget innerModel innerMsg Int
    -> WrappedBoundListWidget innerModel innerMsg Data.Data
dataOfIntBindingWrapper =
    makeListBindingWrapper (Binding.alwaysOk Data.Int) intOfData


intOfDataBindingWrapper :
    BoundListWidget innerModel innerMsg Data.Data
    -> WrappedBoundListWidget innerModel innerMsg Int
intOfDataBindingWrapper =
    makeListBindingWrapper intOfData (Binding.alwaysOk Data.Int)


makeListBindingFilter :
    (carriedValue -> Bool)
    -> BoundListWidget innerModel innerMsg carriedValue
    -> WrappedBoundListWidget innerModel innerMsg carriedValue
makeListBindingFilter p =
    let
        filter v =
            if p v then
                Binding.Ok v
            else
                trivialErr "makeListBindingFilter: filter not satisfied"
    in
        makeListBindingWrapper Binding.Ok filter
