port module LocalStorage
    exposing
        ( getDataSub
        , setDataCmd
        , getStringSub
        , setStringCmd
        , itemAddedSub
        , itemRemovedSub
        , addItemCmd
        , modifyItemCmd
        , removeItemCmd
        , askDataCmd
        , subscribeCmd
        , DataID
        , DataPath
        )

import Data exposing (AttributeValue, Data, Object)
import Json.Decode
import Json.Encode
import Platform.Sub


type alias DataID =
    String


type alias DataPath =
    List String


port getDataSubPort : (( DataID, Json.Encode.Value, Json.Encode.Value, DataPath ) -> msg) -> Sub msg


{-| [getDataSub] takes four parameters:
 1. id of the data
 2. shallow representation of the data (e.g., for a list, the list of UUIDs)
 3. deep representation of the data (e.g., for a list, the list of recursively resolved UUIDs)
 4. path to the subitem which was modified

 If the data is of atomic type (int, string or bool) then 2 and 3 are the same and 4 is just the empty path.
-}
getDataSub : (DataID -> Result String AttributeValue -> Result String AttributeValue -> DataPath -> a) -> Sub a
getDataSub msgBuilder =
    let
        objOfJson json =
            Json.Decode.decodeValue Data.attributeDecoder json
    in
        getDataSubPort (\( id, shallowJson, deepJson, path ) -> msgBuilder id (objOfJson shallowJson) (objOfJson deepJson) path)


getStringSub : (DataID -> String -> a) -> Sub a
getStringSub msgBuilder =
    getDataSub
        (\id _ res _ ->
            case res of
                Ok (Data.SingleData (Just (Data.String s))) ->
                    msgBuilder id s

                _ ->
                    msgBuilder id "FIXME: proper handling of Ids which actually do not contain strings"
        )


port setDataCmdPort : ( DataID, Json.Encode.Value ) -> Cmd msg


setDataCmd : DataID -> Data -> Cmd msg
setDataCmd id d =
    setDataCmdPort ( id, Data.jsonOfData d )


setStringCmd : DataID -> String -> Cmd msg
setStringCmd id s =
    setDataCmd id (Data.String s)


port itemAddedSubPort : (( DataID, DataPath, Json.Encode.Value ) -> msg) -> Sub msg


itemAddedSub : (DataID -> DataPath -> Result String Data -> c) -> Sub c
itemAddedSub msgBuilder =
    let
        objOfJson json =
            Json.Decode.decodeValue Data.dataDecoder json
    in
        itemAddedSubPort (\( containerID, i, json ) -> msgBuilder containerID i (objOfJson json))


port askDataCmdPort : DataID -> Cmd msg


askDataCmd : DataID -> Cmd msg
askDataCmd =
    askDataCmdPort


port itemRemovedSubPort : (( DataID, DataPath ) -> msg) -> Sub msg


itemRemovedSub : (DataID -> DataPath -> c) -> Sub c
itemRemovedSub msgBuilder =
    itemRemovedSubPort (\( id, i ) -> msgBuilder id i)


port addItemCmdPort : ( DataID, DataPath, Json.Encode.Value ) -> Cmd msg


addItemCmd : DataID -> DataPath -> Data -> Cmd mssg
addItemCmd id i d =
    addItemCmdPort ( id, i, Data.jsonOfData d )


port subscribeCmdPort : DataID -> Cmd msg


subscribeCmd : DataID -> Cmd mssg
subscribeCmd =
    subscribeCmdPort


port modifyItemCmdPort : ( DataID, DataPath, Json.Encode.Value ) -> Cmd msg


modifyItemCmd : DataID -> DataPath -> Data -> Cmd mssg
modifyItemCmd id i d =
    addItemCmdPort ( id, i, Data.jsonOfData d )


port removeItemCmdPort : ( DataID, DataPath ) -> Cmd msg


removeItemCmd : DataID -> DataPath -> Cmd msg
removeItemCmd id i =
    removeItemCmdPort ( id, i )
