module DataID exposing (..)

import DataType exposing (GenericField(..), Path)
import ListUtils


type alias DataID =
    Path


getChildIdentifier : DataID -> String -> DataID
getChildIdentifier id fieldName =
    Field fieldName :: id


getItemIdentifier : DataID -> Int -> DataID
getItemIdentifier id n =
    Index n :: id


isItemOf : DataID -> DataID -> Maybe Int
isItemOf candidateId id =
    case ListUtils.substract candidateId id of
        Just [ Index i ] ->
            Just i

        _ ->
            Nothing


isChildOf : DataID -> DataID -> Maybe String
isChildOf candidateId id =
    case ListUtils.substract candidateId id of
        Just [ Field fieldName ] ->
            Just fieldName

        _ ->
            Nothing


type alias DataSelector =
    DataID -> DataID
