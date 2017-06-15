module CollectionBinding exposing (..)

import Binding exposing (BindingResult, alwaysOk, filterIrrelevant, trivialErr)
import Data exposing (AttributeValue(..), Data)
import DataManager exposing (DataID)
import Utils exposing (Index)
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
                AddItem (Binding.ofMaybe (f p) errMsg |> Binding.map (\fp -> ( fp, v )))

            RemoveItem (Binding.Ok p) ->
                RemoveItem (Binding.ofMaybe (f p) errMsg)

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
                        (filterIrrelevant (collectionID == boundId) <|
                            case maybeObj of
                                Result.Ok obj ->
                                    Binding.Ok ( i, obj )

                                Result.Err err ->
                                    trivialErr err
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
                                            Binding.ofMaybe (String.toInt s |> Result.toMaybe) "Path is not an index: please report"
                                                |> Binding.map (\i -> ( i, obj ))

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
                (\collectionID i -> buildMsg (filterIrrelevant (collectionID == boundId) (Binding.Ok i)))
    , addItem = \i m -> DataManager.addItemCmd boundId i m
    , modifyItem = \i m -> DataManager.modifyItemCmd boundId i m
    , removeItem = \i -> DataManager.removeItemCmd boundId i
    , getFullList =
        \buildMsg ->
            DataManager.getDataSub
                (\id _ maybeObj path ->
                    buildMsg <|
                        filterIrrelevant (id == boundId && path == []) <|
                            case maybeObj of
                                Result.Ok (MultipleData datas) ->
                                    Binding.Ok datas

                                Result.Ok _ ->
                                    trivialErr "A list was expected but the storage contains something else: please report"

                                Result.Err err ->
                                    trivialErr err
                )
    , ask = DataManager.askDataCmd boundId
    }