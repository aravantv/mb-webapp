port module LocalStorage
    exposing
        ( getDataSub
        , setDataCmd
        , getStringSub
        , setStringCmd
        , itemAddedSub
        , itemRemovedSub
        , addItemCmd
        , removeItemCmd
        , askDataCmd
        , DataID
        )

import Data exposing (AttributeValue, Data, Object)
import Json.Decode
import Json.Encode
import Platform.Sub
import Utils exposing (Index)


type alias DataID =
    String


port getDataSubPort : (( DataID, Json.Encode.Value ) -> msg) -> Sub msg


getDataSub : (DataID -> Result String AttributeValue -> a) -> Sub a
getDataSub msgBuilder =
    let
        objOfJson json =
            Json.Decode.decodeValue Data.attributeDecoder json
    in
        getDataSubPort (\( id, json ) -> msgBuilder id (objOfJson json))


getStringSub : (DataID -> String -> a) -> Sub a
getStringSub msgBuilder =
    getDataSub
        (\id res ->
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


port itemAddedSubPort : (( DataID, Index, Json.Encode.Value, DataID ) -> msg) -> Sub msg


itemAddedSub : (DataID -> Index -> Result String Data -> DataID -> c) -> Sub c
itemAddedSub msgBuilder =
    let
        objOfJson json =
            Json.Decode.decodeValue Data.dataDecoder json
    in
        itemAddedSubPort (\( containerID, i, json, addedEltID ) -> msgBuilder containerID i (objOfJson json) addedEltID)


port askDataCmdPort : DataID -> Cmd msg


askDataCmd : DataID -> Cmd msg
askDataCmd =
    askDataCmdPort


port itemRemovedSubPort : (( DataID, Index ) -> msg) -> Sub msg


itemRemovedSub : (DataID -> Index -> c) -> Sub c
itemRemovedSub msgBuilder =
    itemRemovedSubPort (\( id, i ) -> msgBuilder id i)


port addItemCmdPort : ( DataID, Index, Json.Encode.Value ) -> Cmd msg


addItemCmd : DataID -> Index -> Data -> Cmd mssg
addItemCmd id i d =
    addItemCmdPort ( id, i, Data.jsonOfData d )


port removeItemCmdPort : ( DataID, Index ) -> Cmd msg


removeItemCmd : DataID -> Index -> Cmd msg
removeItemCmd id i =
    removeItemCmdPort ( id, i )
