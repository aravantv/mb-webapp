port module LocalStorageResource exposing (..)

import Path exposing (..)
import Json.Encode exposing (..)


port setValue : ( String, Json.Encode.Value ) -> Cmd msg


symbolicPathToString : SymbolicPath String -> String
symbolicPathToString ( path, fields ) =
    path ++ String.join "." (List.map stringOfGenericField fields)


commandBuilder : (a -> Json.Encode.Value) -> SymbolicPath String -> a -> Cmd msg
commandBuilder toJson path value =
    setValue ( symbolicPathToString path, toJson value )


commandBuilderString : SymbolicPath String -> String -> Cmd msg
commandBuilderString =
    commandBuilder Json.Encode.string


commandBuilderInt : SymbolicPath String -> Int -> Cmd msg
commandBuilderInt =
    commandBuilder Json.Encode.int
