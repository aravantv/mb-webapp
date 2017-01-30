port module LocalStorage exposing (getStringSub, setStringCmd, itemAddedSub, itemRemovedSub, addItemCmd, removeItemCmd, getItemContentCmd)

import Platform.Sub
import Widget exposing (genericFieldOfString, stringOfGenericField)


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
storagePathOfWidgetPath : Widget.Path -> StoragePath
storagePathOfWidgetPath p =
    List.reverse (List.map stringOfGenericField p)


widgetPathOfStoragePath : StoragePath -> Widget.Path
widgetPathOfStoragePath sp =
    List.reverse (List.map genericFieldOfString sp)


port getStringSubPort : (( StoragePath, JsonString ) -> msg) -> Sub msg


getStringSub : (( Widget.Path, JsonString ) -> a) -> Sub a
getStringSub msgBuilder =
    getStringSubPort (\( p, s ) -> msgBuilder ( widgetPathOfStoragePath p, s ))


port setStringCmdPort : ( StoragePath, String ) -> Cmd msg


setStringCmd : ( Widget.Path, String ) -> Cmd msg
setStringCmd ( p, s ) =
    setStringCmdPort ( storagePathOfWidgetPath p, s )


port itemAddedSubPort : (StoragePath -> msg) -> Sub msg


itemAddedSub : (Widget.Path -> c) -> Sub c
itemAddedSub msgBuilder =
    itemAddedSubPort (msgBuilder << widgetPathOfStoragePath)


port getItemContentCmdPort : StoragePath -> Cmd msg


getItemContentCmd : Widget.Path -> Cmd msg
getItemContentCmd p =
    getItemContentCmdPort (storagePathOfWidgetPath p)


port itemRemovedSubPort : (StoragePath -> msg) -> Sub msg


itemRemovedSub : (Widget.Path -> c) -> Sub c
itemRemovedSub msgBuilder =
    itemRemovedSubPort (msgBuilder << widgetPathOfStoragePath)


{-| addItemCmd literally adds an item: it inserts an element at the given path but does not fill it in with any value!
-}
port addItemCmdPort : StoragePath -> Cmd msg


addItemCmd : Widget.Path -> Cmd msg
addItemCmd =
    addItemCmdPort << storagePathOfWidgetPath


port removeItemCmdPort : StoragePath -> Cmd msg


removeItemCmd : Widget.Path -> Cmd msg
removeItemCmd =
    removeItemCmdPort << storagePathOfWidgetPath
