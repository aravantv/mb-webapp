module CollectionBinding exposing (..)

import Binding exposing (BindingResult, alwaysOk)
import ConstraintUtils exposing (Fixes(..), UnfulfillmentInfo, trivialUnfulfillmentInfo)
import Data exposing (AttributeValue(..), Data)
import DataManager exposing (DataID)
import Html exposing (sub)
import IndexMapping exposing (IndexMapping)
import Widget exposing (TopWidget, Widget, cmdOf, cmdOfMsg, mapParamsSub, mapParamsUp, modelOf)


mapItemAdded :
    (fromValue -> BindingResult toValue)
    -> BindingResult ( collectionPath, fromValue )
    -> BindingResult ( collectionPath, toValue )
mapItemAdded out2in =
    Binding.andThen (\( i, v ) -> Binding.map (\n -> ( i, n )) (out2in v))


type CollectionBindingUpInfo collectionPath carriedValue
    = AddItem (BindingResult ( collectionPath, carriedValue ))
    | RemoveItem (BindingResult collectionPath)
    | DoNothing


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
            AddItem (mapItemAdded f res)

        RemoveItem res ->
            RemoveItem res

        DoNothing ->
            DoNothing


type alias CollectionBindingSubInfo collectionPath carriedValue msg =
    { itemAdded : BindingResult ( collectionPath, carriedValue, DataID ) -> msg
    , itemRemoved : BindingResult collectionPath -> msg
    }


type alias BoundCollectionWidget collectionPath model msg carriedValue =
    Widget (CollectionBindingUpInfo collectionPath carriedValue) (CollectionBindingSubInfo collectionPath carriedValue msg) model msg


type alias BoundListWidget model msg carriedValue =
    BoundCollectionWidget Index model msg carriedValue


type alias CollectionBinding collectionPath msg carriedValue =
    { addItem : collectionPath -> carriedValue -> Cmd msg
    , removeItem : collectionPath -> Cmd msg
    , itemAdded : (BindingResult ( collectionPath, carriedValue, DataID ) -> msg) -> Sub msg
    , itemRemoved : (BindingResult collectionPath -> msg) -> Sub msg
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

                    fullListRetrieval id shallowVal attrVal =
                        case ( shallowVal, attrVal ) of
                            ( Ok (MultipleData idsAsAttrs), Ok (MultipleData datas) ) ->
                                let
                                    ids =
                                        List.filterMap
                                            (\d ->
                                                case d of
                                                    Data.String s ->
                                                        Just s

                                                    _ ->
                                                        Nothing
                                            )
                                            idsAsAttrs

                                    id2data =
                                        List.map2 (\x y -> ( x, y )) ids datas
                                in
                                    Init <|
                                        List.indexedMap (\i ( id, data ) -> mapper.itemAdded <| Binding.Ok ( i, data, id )) id2data

                            _ ->
                                Nop
                in
                    ( Sub.batch
                        (embedSub sub
                            :: [ embedSub (b.itemAdded mapper.itemAdded)
                               , embedSub (b.itemRemoved mapper.itemRemoved)
                               , DataManager.getDataSub fullListRetrieval
                               ]
                        )
                    , ()
                    )
        , view = \model -> Html.map Delegate (w.view model)
        }


type alias Index =
    Int


listBinding :
    DataID
    -> CollectionBinding Index msg Data.Data
listBinding boundId =
    { itemAdded =
        \f ->
            DataManager.itemAddedSub
                (\collectionID i maybeObj addedEltID ->
                    f
                        (if collectionID == boundId then
                            case maybeObj of
                                Result.Ok obj ->
                                    Binding.Ok ( i, obj, addedEltID )

                                Result.Err err ->
                                    Binding.Err { unfulfillmentDescription = err, fixes = PossibleFixes [] }
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
    , removeItem = \i -> DataManager.removeItemCmd boundId i
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
    in
        { init = ( ( modelOf w.init, IndexMapping.empty ), Cmd.map trivialMsg (cmdOf w.init) )
        , update =
            \msg ( model, idxMap ) ->
                let
                    ( subMsg, newIdxMap ) =
                        msg idxMap

                    ( newModel, cmd, info ) =
                        w.update subMsg model
                in
                    ( ( newModel, newIdxMap ), Cmd.map trivialMsg cmd, mapUpInfo in2out info )
        , subscriptions =
            \( model, _ ) ->
                let
                    ( sub, info ) =
                        w.subscriptions model

                    newInfo =
                        { itemAdded =
                            \res ->
                                case res of
                                    Binding.Ok ( i, s, id ) ->
                                        case out2in s of
                                            Binding.Ok n ->
                                                \idxMap ->
                                                    let
                                                        newIdxMap =
                                                            IndexMapping.insert idxMap i

                                                        res =
                                                            case IndexMapping.get newIdxMap i of
                                                                Just j ->
                                                                    Binding.Ok ( j, n, id )

                                                                Nothing ->
                                                                    Binding.Err <| trivialUnfulfillmentInfo "Index not found - please report"
                                                    in
                                                        ( info.itemAdded res, newIdxMap )

                                            Binding.Err err ->
                                                \idxMap ->
                                                    ( info.itemAdded (Binding.Err err)
                                                    , IndexMapping.insertButSkip idxMap i
                                                    )

                                            Binding.Irrelevant ->
                                                \idxMap ->
                                                    ( info.itemAdded Binding.Irrelevant
                                                    , IndexMapping.insertButSkip idxMap i
                                                    )

                                    Binding.Err err ->
                                        trivialMsg (info.itemAdded (Binding.Err err))

                                    Binding.Irrelevant ->
                                        trivialMsg (info.itemAdded Binding.Irrelevant)
                        , itemRemoved =
                            \res ->
                                let
                                    msg idxMap =
                                        case res of
                                            Binding.Ok i ->
                                                IndexMapping.remove idxMap i

                                            _ ->
                                                idxMap
                                in
                                    \idxMap -> ( info.itemRemoved res, msg idxMap )
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
