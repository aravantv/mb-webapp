module DataManager exposing (..)

import Data exposing (Data(..))
import LocalStorage
import Utils exposing (Index)


type alias DataID =
    LocalStorage.DataID


getStringSub : (DataID -> String -> msg) -> Sub msg
getStringSub f =
    LocalStorage.getStringSub f


setStringCmd : DataID -> String -> Cmd msg
setStringCmd id s =
    LocalStorage.setStringCmd id s


itemAddedSub : (DataID -> Index -> Result String Data -> msg) -> Sub msg
itemAddedSub f =
    LocalStorage.itemAddedSub f


itemRemovedSub : (DataID -> Index -> msg) -> Sub msg
itemRemovedSub f =
    LocalStorage.itemRemovedSub f


addItemCmd : DataID -> Index -> Data -> Cmd msg
addItemCmd id i d =
    LocalStorage.addItemCmd id i d


removeItemCmd : DataID -> Index -> Cmd msg
removeItemCmd id i =
    LocalStorage.removeItemCmd id i


askDataCmd : DataID -> Cmd msg
askDataCmd id =
    LocalStorage.askDataCmd id
