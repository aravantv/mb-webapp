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

import Json.Decode
import Json.Encode
import MetaModel exposing (ClassRef, MetaModel, ModelElementIdentifier, ModelType, genericFieldOfString, stringOfGenericField)
import Model exposing (Object)
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
storagePathOfWidgetPath : ModelElementIdentifier -> StoragePath
storagePathOfWidgetPath p =
    List.reverse (List.map stringOfGenericField p)


widgetPathOfStoragePath : StoragePath -> ModelElementIdentifier
widgetPathOfStoragePath sp =
    List.reverse (List.map genericFieldOfString sp)


port getStringSubPort : (( StoragePath, JsonString ) -> msg) -> Sub msg


getStringSub : (( ModelElementIdentifier, JsonString ) -> a) -> Sub a
getStringSub msgBuilder =
    getStringSubPort (\( p, s ) -> msgBuilder ( widgetPathOfStoragePath p, s ))


port setStringCmdPort : ( StoragePath, String ) -> Cmd msg


setStringCmd : ( ModelElementIdentifier, String ) -> Cmd msg
setStringCmd ( p, s ) =
    setStringCmdPort ( storagePathOfWidgetPath p, s )


port itemAddedSubPort : (( StoragePath, Json.Encode.Value ) -> msg) -> Sub msg


itemAddedSub : MetaModel -> ModelType -> (( ModelElementIdentifier, Result String Model.Model ) -> c) -> Sub c
itemAddedSub mm ty msgBuilder =
    let
        objOfJson json =
            Json.Decode.decodeValue (Model.modelDecoder mm ty) json
    in
        itemAddedSubPort (\( sp, json ) -> msgBuilder ( widgetPathOfStoragePath sp, objOfJson json ))


port askContentCmdPort : StoragePath -> Cmd msg


askContentCmd : ModelElementIdentifier -> Cmd msg
askContentCmd p =
    askContentCmdPort (storagePathOfWidgetPath p)


port itemRemovedSubPort : (StoragePath -> msg) -> Sub msg


itemRemovedSub : (ModelElementIdentifier -> c) -> Sub c
itemRemovedSub msgBuilder =
    itemRemovedSubPort (msgBuilder << widgetPathOfStoragePath)


{-| addItemCmd literally adds an item: it inserts an element at the given path but does not fill it in with any value!
-}
port addItemCmdPort : ( StoragePath, Json.Encode.Value ) -> Cmd msg


addItemCmd : ModelElementIdentifier -> Model.Model -> Cmd msg
addItemCmd p m =
    addItemCmdPort ( storagePathOfWidgetPath p, Model.jsonOfModel m )


port removeItemCmdPort : StoragePath -> Cmd msg


removeItemCmd : ModelElementIdentifier -> Cmd msg
removeItemCmd =
    removeItemCmdPort << storagePathOfWidgetPath
