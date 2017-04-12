port module LocalStorage
    exposing
        ( getStringSub
        , setStringCmd
        , itemAddedSub
        , itemRemovedSub
        , addItemCmd
        , removeItemCmd
        , askContentCmd
        )

import Data exposing (Object, Data)
import DataID exposing (DataID)
import DataType exposing (ClassRef, DataType, DataTypeSet, genericFieldOfString, stringOfGenericField)
import Json.Decode
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


itemAddedSub : DataTypeSet -> (( DataID, Result String Data ) -> c) -> Sub c
itemAddedSub dts msgBuilder =
    let
        objOfJson json =
            Json.Decode.decodeValue (Data.dataDecoder dts) json
    in
        itemAddedSubPort (\( sp, json ) -> msgBuilder ( widgetPathOfStoragePath sp, objOfJson json ))


port askContentCmdPort : StoragePath -> Cmd msg


askContentCmd : DataID -> Cmd msg
askContentCmd p =
    askContentCmdPort (storagePathOfWidgetPath p)


port itemRemovedSubPort : (StoragePath -> msg) -> Sub msg


itemRemovedSub : (DataID -> c) -> Sub c
itemRemovedSub msgBuilder =
    itemRemovedSubPort (msgBuilder << widgetPathOfStoragePath)


{-| addItemCmd literally adds an item: it inserts an element at the given path but does not fill it in with any value!
-}
port addItemCmdPort : ( StoragePath, Json.Encode.Value ) -> Cmd msg


addItemCmd : DataID -> Data -> Cmd msg
addItemCmd p d =
    addItemCmdPort ( storagePathOfWidgetPath p, Data.jsonOfData d )


port removeItemCmdPort : StoragePath -> Cmd msg


removeItemCmd : DataID -> Cmd msg
removeItemCmd =
    removeItemCmdPort << storagePathOfWidgetPath
