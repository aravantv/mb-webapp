module DataID exposing (..)

import ListUtils


type GenericField
    = Field String
    | Index Int


stringOfGenericField : GenericField -> String
stringOfGenericField f =
    case f of
        Field s ->
            s

        Index i ->
            toString i


genericFieldOfString : String -> GenericField
genericFieldOfString s =
    case String.toInt s of
        Ok n ->
            Index n

        Err _ ->
            Field s


{-| Paths are provided as a list of string: the root is the *last* element.
-}
type alias DataID =
    List GenericField


getChildIdentifier : DataID -> String -> DataID
getChildIdentifier id fieldName =
    Field fieldName :: id


getItemIdentifier : DataID -> Int -> DataID
getItemIdentifier id n =
    Index n :: id


itemOf : DataID -> DataID -> Maybe Int
itemOf candidateId id =
    case ListUtils.substract id candidateId of
        Just [ Index i ] ->
            Just i

        _ ->
            Nothing


isItemOf : DataID -> DataID -> Bool
isItemOf candidateId id =
    itemOf candidateId id /= Nothing


isChildOf : DataID -> DataID -> Maybe String
isChildOf candidateId id =
    case ListUtils.substract candidateId id of
        Just [ Field fieldName ] ->
            Just fieldName

        _ ->
            Nothing


type alias DataSelector =
    DataID -> DataID
