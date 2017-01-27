port module Storage exposing (getStringSub, setStringCmd, itemAddedSub, itemRemovedSub, addItemCmd, removeItemCmd)

import Platform.Sub
import Widget


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
pathStorageOfPath : Widget.Path -> StoragePath
pathStorageOfPath p =
    List.reverse p


port getStringSubPort : (( StoragePath, JsonString ) -> msg) -> Sub msg


getStringSub : (( StoragePath, JsonString ) -> a) -> Sub a
getStringSub msgBuilder =
    getStringSubPort (\( p, s ) -> msgBuilder ( pathStorageOfPath p, s ))


port setStringCmdPort : ( StoragePath, String ) -> Cmd msg


setStringCmd : ( Widget.Path, String ) -> Cmd msg
setStringCmd ( p, s ) =
    setStringCmdPort ( pathStorageOfPath p, s )


port itemAddedSubPort : (StoragePath -> msg) -> Sub msg


itemAddedSub : (StoragePath -> c) -> Sub c
itemAddedSub msgBuilder =
    itemAddedSubPort (msgBuilder << pathStorageOfPath)


port itemRemovedSubPort : (StoragePath -> msg) -> Sub msg


itemRemovedSub : (StoragePath -> c) -> Sub c
itemRemovedSub msgBuilder =
    itemRemovedSubPort (msgBuilder << pathStorageOfPath)


{-| addItemCmd literally adds an item: it inserts an element at the given path but does not fill it in with any value!
-}
port addItemCmdPort : StoragePath -> Cmd msg


addItemCmd : Widget.Path -> Cmd msg
addItemCmd =
    addItemCmdPort << pathStorageOfPath


port removeItemCmdPort : StoragePath -> Cmd msg


removeItemCmd : Widget.Path -> Cmd msg
removeItemCmd =
    removeItemCmdPort << pathStorageOfPath
