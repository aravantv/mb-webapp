port module LocalStorage
    exposing
        ( getStringSub
        , setStringCmd
        , itemAddedSub
        , itemRemovedSub
        , addItemCmd
        , removeItemCmd
        , askDataCmd
        )

import Data exposing (Data, Object)
import DataID exposing (DataID, genericFieldOfString, stringOfGenericField)
import Json.Decode
import Utils exposing (Index)
import Json.Encode
import Platform.Sub


{-| Storage paths are different than widget paths: they are ordered more intuitively,
  i.e., the root is the *first* element. That requires reverting before calling the port though.
-}
type alias StoragePath =
    List String


fieldOfIndex : Int -> String
fieldOfIndex i =
    toString i


type alias JsonString =
    String


{-| Just reverts a path
-}
storagePathOfWidgetPath : DataID -> StoragePath
storagePathOfWidgetPath p =
    List.reverse (List.map stringOfGenericField p)


widgetPathOfStoragePath : StoragePath -> DataID
widgetPathOfStoragePath sp =
    List.reverse (List.map genericFieldOfString sp)


port getStringSubPort : (( StoragePath, JsonString ) -> msg) -> Sub msg


getStringSub : (( DataID, JsonString ) -> a) -> Sub a
getStringSub msgBuilder =
    getStringSubPort (\( p, s ) -> msgBuilder ( widgetPathOfStoragePath p, s ))


port setStringCmdPort : ( StoragePath, String ) -> Cmd msg


setStringCmd : ( DataID, String ) -> Cmd msg
setStringCmd ( p, s ) =
    setStringCmdPort ( storagePathOfWidgetPath p, s )


port itemAddedSubPort : (( StoragePath, Json.Encode.Value ) -> msg) -> Sub msg


itemAddedSub : (( DataID, Result String Data ) -> c) -> Sub c
itemAddedSub msgBuilder =
    let
        objOfJson json =
            Json.Decode.decodeValue Data.dataDecoder json
    in
        itemAddedSubPort (\( sp, json ) -> msgBuilder ( widgetPathOfStoragePath sp, objOfJson json ))


port askDataCmdPort : StoragePath -> Cmd msg


askDataCmd : DataID -> Cmd msg
askDataCmd p =
    askDataCmdPort (storagePathOfWidgetPath p)


port itemRemovedSubPort : (StoragePath -> msg) -> Sub msg


itemRemovedSub : (DataID -> c) -> Sub c
itemRemovedSub msgBuilder =
    itemRemovedSubPort (msgBuilder << widgetPathOfStoragePath)


port addItemCmdPort : ( StoragePath, Index, Json.Encode.Value ) -> Cmd msg


addItemCmd : DataID -> Index -> Data -> Cmd mssg
addItemCmd p i d =
    addItemCmdPort ( storagePathOfWidgetPath p, i, Data.jsonOfData d )


port removeItemCmdPort : StoragePath -> Cmd msg


removeItemCmd : DataID -> Cmd msg
removeItemCmd =
    removeItemCmdPort << storagePathOfWidgetPath
