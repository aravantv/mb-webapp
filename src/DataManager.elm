module DataManager exposing (..)

import Data exposing (Data(..))
import DataID exposing (DataID)
import DataType exposing (DataTypeSet)
import LocalStorage


getStringSub : (( DataID, String ) -> msg) -> Sub msg
getStringSub f =
    LocalStorage.getStringSub f


setStringCmd : ( DataID, String ) -> Cmd msg
setStringCmd ( id, s ) =
    LocalStorage.setStringCmd ( id, s )


itemAddedSub : DataTypeSet -> (( DataID, Result String Data ) -> msg) -> Sub msg
itemAddedSub dts f =
    LocalStorage.itemAddedSub dts f


itemRemovedSub : (DataID -> msg) -> Sub msg
itemRemovedSub f =
    LocalStorage.itemRemovedSub f


addItemCmd : DataID -> Data -> Cmd msg
addItemCmd id d =
    LocalStorage.addItemCmd id d


removeItemCmd : DataID -> Cmd msg
removeItemCmd id =
    LocalStorage.removeItemCmd id
