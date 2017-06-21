module CollectionBinding exposing (..)

import BindingResult exposing (BindingResult, alwaysOk, filterIrrelevant, trivialErr)
import Data exposing (AttributeValue(..), Data)
import DataManager exposing (DataID)
import Widget exposing (TopWidget, Widget, cmdOf, cmdOfMsg, mapParamsSub, mapParamsUp, modelOf)


mapIndexValueResult :
    (fromValue -> BindingResult toValue)
    -> BindingResult ( Path, fromValue )
    -> BindingResult ( Path, toValue )
mapIndexValueResult out2in =
    BindingResult.andThen (\( p, v ) -> BindingResult.map (\n -> ( p, n )) (out2in v))


type alias Path =
    List String


type BindingSymbolicCmd carriedValue
    = Add (BindingResult ( Path, carriedValue ))
    | Remove (BindingResult Path)
    | Modify (BindingResult ( Path, carriedValue ))
    | DoNothing


mapPath : (Path -> Maybe Path) -> BindingSymbolicCmd carriedValue -> BindingSymbolicCmd carriedValue
mapPath f info =
    let
        errMsg =
            "mapPath: Path could not be transformed"
    in
        case info of
            Add (BindingResult.Ok ( p, v )) ->
                Add (BindingResult.ofMaybe (f p) errMsg |> BindingResult.map (\fp -> ( fp, v )))

            Remove (BindingResult.Ok p) ->
                Remove (BindingResult.ofMaybe (f p) errMsg)

            x ->
                x


doNothing : a -> ( a, Cmd msg, BindingSymbolicCmd carriedValue )
doNothing x =
    ( x, Cmd.none, DoNothing )


mapBindingSymbolicCmd :
    (carriedFromValue -> BindingResult carriedToValue)
    -> BindingSymbolicCmd carriedFromValue
    -> BindingSymbolicCmd carriedToValue
mapBindingSymbolicCmd f i =
    case i of
        Add res ->
            Add (mapIndexValueResult f res)

        Modify res ->
            Modify (mapIndexValueResult f res)

        Remove res ->
            Remove res

        DoNothing ->
            DoNothing


type alias BindingSymbolicSub carriedValue msg =
    { added : BindingResult ( Path, carriedValue ) -> msg
    , removed : BindingResult Path -> msg
    , modified : BindingResult ( Path, carriedValue ) -> msg
    }


type alias BoundCollectionWidget model msg carriedValue =
    Widget (BindingSymbolicCmd carriedValue) (BindingSymbolicSub carriedValue msg) model msg


type alias Binding msg carriedValue =
    { addCmd : Path -> carriedValue -> Cmd msg
    , removeCmd : Path -> Cmd msg
    , modifyCmd : Path -> carriedValue -> Cmd msg
    , addedSub : (BindingResult ( Path, carriedValue ) -> msg) -> Sub msg
    , modifiedSub : (BindingResult ( Path, carriedValue ) -> msg) -> Sub msg
    , removedSub : (BindingResult Path -> msg) -> Sub msg
    , askCmd : Cmd msg
    }


applyBinding : Binding msg Data -> BoundCollectionWidget model msg Data -> Widget () () model msg
applyBinding b w =
    { init = ( modelOf w.init, Cmd.batch [ cmdOf w.init, b.askCmd ] )
    , update =
        \msg model ->
            let
                ( newModel, cmd, symbolicCmd ) =
                    w.update msg model

                maybeConcreteCmd =
                    case symbolicCmd of
                        Add (BindingResult.Ok ( p, val )) ->
                            Just (b.addCmd p val)

                        Modify (BindingResult.Ok ( p, val )) ->
                            Just (b.modifyCmd p val)

                        Remove (BindingResult.Ok p) ->
                            Just (b.removeCmd p)

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
                    [ b.addedSub symbolicSub.added
                    , b.removedSub symbolicSub.removed
                    ]
            in
                ( Sub.batch (sub :: concreteSubs), () )
    , view = w.view
    }


listBinding : DataID -> Binding msg Data.Data
listBinding boundId =
    { addedSub =
        \buildMsg ->
            DataManager.itemAddedSub
                (\collectionID i maybeObj ->
                    buildMsg
                        (filterIrrelevant (collectionID == boundId) <|
                            case maybeObj of
                                Result.Ok obj ->
                                    BindingResult.Ok ( i, obj )

                                Result.Err err ->
                                    trivialErr err
                        )
                )
    , modifiedSub =
        \buildMsg ->
            DataManager.getDataSub
                (\id _ maybeObj path ->
                    buildMsg
                        (if id == boundId then
                            case maybeObj of
                                Result.Ok (SingleData (Just obj)) ->
                                    case path of
                                        [ s ] ->
                                            BindingResult.ofMaybe (String.toInt s |> Result.toMaybe) "Path is not an index: please report"
                                                |> BindingResult.map (\i -> ( i, obj ))

                                        _ ->
                                            BindingResult.Irrelevant

                                Result.Ok _ ->
                                    trivialErr "A list was expected but the storage contains something else: please report"

                                Result.Err err ->
                                    trivialErr err
                         else
                            BindingResult.Irrelevant
                        )
                )
    , removedSub =
        \buildMsg ->
            DataManager.itemRemovedSub
                (\collectionID i -> buildMsg (filterIrrelevant (collectionID == boundId) (BindingResult.Ok i)))
    , addCmd = \i m -> DataManager.addItemCmd boundId i m
    , modifyCmd = \i m -> DataManager.modifyItemCmd boundId i m
    , removeCmd = \i -> DataManager.removeItemCmd boundId i
    , askCmd = DataManager.askDataCmd boundId
    }
