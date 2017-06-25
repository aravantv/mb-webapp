module DataManager exposing (..)

import Data exposing (AttributeValue, Data(..))
import LocalStorage


type alias DataID =
    LocalStorage.DataID


type alias DataPath =
    LocalStorage.DataPath


getDataSub : (DataID -> Result String AttributeValue -> Result String AttributeValue -> DataPath -> msg) -> Sub msg
getDataSub f =
    LocalStorage.getDataSub f


setDataCmd : DataID -> Data -> Cmd msg
setDataCmd id s =
    LocalStorage.setDataCmd id s


getStringSub : (DataID -> String -> msg) -> Sub msg
getStringSub f =
    LocalStorage.getStringSub f


setStringCmd : DataID -> String -> Cmd msg
setStringCmd id s =
    LocalStorage.setStringCmd id s


addedSub : (DataID -> DataPath -> Result String Data -> msg) -> Sub msg
addedSub f =
    LocalStorage.itemAddedSub f


removedSub : (DataID -> DataPath -> msg) -> Sub msg
removedSub f =
    LocalStorage.itemRemovedSub f


addItemCmd : DataID -> DataPath -> Data -> Cmd msg
addItemCmd id i d =
    LocalStorage.addItemCmd id i d


modifyItemCmd : DataID -> DataPath -> Data -> Cmd msg
modifyItemCmd id i d =
    LocalStorage.modifyItemCmd id i d


removeItemCmd : DataID -> DataPath -> Cmd msg
removeItemCmd id i =
    LocalStorage.removeItemCmd id i


askDataCmd : DataID -> Cmd msg
askDataCmd id =
    LocalStorage.askDataCmd id


subscribeCmd : DataID -> Cmd msg
subscribeCmd id =
    LocalStorage.subscribeCmd id
