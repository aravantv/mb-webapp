module DataManager exposing (..)

import Data exposing (Data(..))
import DataID exposing (DataID)
import LocalStorage


getStringSub : (( DataID, String ) -> msg) -> Sub msg
getStringSub f =
    LocalStorage.getStringSub f


setStringCmd : ( DataID, String ) -> Cmd msg
setStringCmd ( id, s ) =
    LocalStorage.setStringCmd ( id, s )


itemAddedSub : (( DataID, Result String Data ) -> msg) -> Sub msg
itemAddedSub f =
    LocalStorage.itemAddedSub f


itemRemovedSub : (DataID -> msg) -> Sub msg
itemRemovedSub f =
    LocalStorage.itemRemovedSub f


addItemCmd : DataID -> Data -> Cmd msg
addItemCmd id d =
    LocalStorage.addItemCmd id d


removeItemCmd : DataID -> Cmd msg
removeItemCmd id =
    LocalStorage.removeItemCmd id


askDataCmd : DataID -> Cmd msg
askDataCmd id =
    LocalStorage.askDataCmd id
