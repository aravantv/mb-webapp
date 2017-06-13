module CollectionBinding exposing (..)

import Binding exposing (BindingResult, alwaysOk)
import ConstraintUtils exposing (Fixes(..), UnfulfillmentInfo, trivialUnfulfillmentInfo)
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


type CollectionBindingUpInfo collectionPath carriedValue
    = AddItem (BindingResult ( collectionPath, carriedValue ))
    | RemoveItem (BindingResult collectionPath)
    | ModifyItem (BindingResult ( collectionPath, carriedValue ))
    | DoNothing


mapCollectionPath :
    (collectionPath -> Maybe collectionPath)
    -> CollectionBindingUpInfo collectionPath carriedValue
    -> CollectionBindingUpInfo collectionPath carriedValue
mapCollectionPath f info =
    let
        errMsg =
            "mapCollectionPath: collectionPath could not be transformed"
    in
        case info of
            AddItem (Binding.Ok ( p, v )) ->
                case f p of
                    Just newP ->
                        AddItem (Binding.Ok ( newP, v ))

                    Nothing ->
                        AddItem (Binding.Err <| trivialUnfulfillmentInfo errMsg)

            RemoveItem (Binding.Ok p) ->
                case f p of
                    Just newP ->
                        RemoveItem (Binding.Ok newP)

                    Nothing ->
                        RemoveItem (Binding.Err <| trivialUnfulfillmentInfo errMsg)

            x ->
                x


doNothing : a -> ( a, Cmd msg, CollectionBindingUpInfo collectionPath carriedValue )
doNothing x =
    ( x, Cmd.none, DoNothing )


mapUpInfo :
    (carriedFromValue -> BindingResult carriedToValue)
    -> CollectionBindingUpInfo collectionPath carriedFromValue
    -> CollectionBindingUpInfo collectionPath carriedToValue
mapUpInfo f i =
    case i of
        AddItem res ->
            AddItem (mapIndexValueResult f res)

        ModifyItem res ->
            ModifyItem (mapIndexValueResult f res)

        RemoveItem res ->
            RemoveItem res

        DoNothing ->
            DoNothing


type alias CollectionBindingSubInfo collectionPath carriedValue msg =
    { itemAdded : BindingResult ( collectionPath, carriedValue ) -> msg
    , itemRemoved : BindingResult collectionPath -> msg
    , itemModified : BindingResult ( collectionPath, carriedValue ) -> msg
    , getFullList : BindingResult (List carriedValue) -> msg
    }


type alias BoundCollectionWidget collectionPath model msg carriedValue =
    Widget (CollectionBindingUpInfo collectionPath carriedValue) (CollectionBindingSubInfo collectionPath carriedValue msg) model msg


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


type Msg subMsg
    = Init (List subMsg)
    | Delegate subMsg
    | Nop


applyListBinding :
    CollectionBinding Index msg Data
    -> BoundCollectionWidget Index model msg Data
    -> Widget () () model (Msg msg)
applyListBinding b w =
    let
        ( newInitModel, newInitCmd ) =
            ( modelOf w.init, Cmd.map Delegate <| Cmd.batch [ cmdOf w.init, b.ask ] )

        updateOneMsg msg ( modelAcc, cmdAcc ) =
            let
                ( newModel, cmd, upInfo ) =
                    w.update msg modelAcc

                newCmd =
                    case upInfo of
                        AddItem (Binding.Ok ( idx, val )) ->
                            Cmd.batch [ cmd, b.addItem idx val ]

                        AddItem _ ->
                            cmd

                        ModifyItem (Binding.Ok ( idx, val )) ->
                            Cmd.batch [ cmd, b.modifyItem idx val ]

                        ModifyItem _ ->
                            cmd

                        RemoveItem (Binding.Ok idx) ->
                            Cmd.batch [ cmd, b.removeItem idx ]

                        RemoveItem _ ->
                            cmd

                        DoNothing ->
                            cmd
            in
                ( newModel, newCmd :: cmdAcc )
    in
        { init = ( newInitModel, newInitCmd )
        , update =
            \msg model ->
                case msg of
                    Nop ->
                        ( model, Cmd.none, () )

                    Delegate subMsg ->
                        let
                            ( newModel, cmds ) =
                                updateOneMsg subMsg ( model, [] )
                        in
                            ( newModel, Cmd.map Delegate (Cmd.batch cmds), () )

                    Init msgs ->
                        let
                            ( updatedModel, allCmds ) =
                                List.foldl updateOneMsg ( newInitModel, [] ) msgs
                        in
                            ( updatedModel, Cmd.map Delegate <| Cmd.batch (cmdOf w.init :: allCmds), () )
        , subscriptions =
            \model ->
                let
                    ( sub, mapper ) =
                        w.subscriptions model

                    embedSub sub =
                        Sub.map Delegate sub
                in
                    ( Sub.batch
                        (embedSub sub
                            :: [ embedSub (b.itemAdded mapper.itemAdded)
                               , embedSub (b.itemRemoved mapper.itemRemoved)
                               , embedSub (b.getFullList mapper.getFullList)
                               ]
                        )
                    , ()
                    )
        , view = \model -> Html.map Delegate (w.view model)
        }


type alias Index =
    Int


listBinding : DataID -> CollectionBinding Index msg Data.Data
listBinding boundId =
    { itemAdded =
        \f ->
            DataManager.itemAddedSub
                (\collectionID i maybeObj ->
                    f
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
        \f ->
            DataManager.getDataSub
                (\id _ maybeObj path ->
                    f
                        (if id == boundId then
                            case maybeObj of
                                Result.Ok (SingleData (Just obj)) ->
                                    case path of
                                        [ s ] ->
                                            case String.toInt s of
                                                Ok i ->
                                                    Binding.Ok ( i, obj )

                                                Err err ->
                                                    Binding.Err <| trivialUnfulfillmentInfo "Path is not an index: please report"

                                        _ ->
                                            Binding.Irrelevant

                                Result.Ok _ ->
                                    Binding.Err <| trivialUnfulfillmentInfo "A list was expected but the storage contains something else: please report"

                                Result.Err err ->
                                    Binding.Err <| trivialUnfulfillmentInfo err
                         else
                            Binding.Irrelevant
                        )
                )
    , itemRemoved =
        \f ->
            DataManager.itemRemovedSub
                (\collectionID i ->
                    f
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
                                    Binding.Err <| trivialUnfulfillmentInfo "A list was expected but the storage contains something else: please report"

                                Result.Err err ->
                                    Binding.Err <| trivialUnfulfillmentInfo err
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
                                        Binding.Err <| trivialUnfulfillmentInfo "Index not found - please report"
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
                    ( ( newModel, newIdxMap ), Cmd.map trivialMsg cmd, mapUpInfo in2out newInfo )
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
                                                        Binding.Err <| trivialUnfulfillmentInfo "Index not found - please report"
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
            Binding.Err (trivialUnfulfillmentInfo "Not a string")


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
            Binding.Err (trivialUnfulfillmentInfo "Not an integer")


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
                Binding.Err <| trivialUnfulfillmentInfo "makeListBindingFilter: filter not satisfied"
    in
        makeListBindingWrapper Binding.Ok filter
