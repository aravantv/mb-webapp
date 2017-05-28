port module LocalStorage
    exposing
        ( getStringSub
        , setStringCmd
        , itemAddedSub
        , itemRemovedSub
        , addItemCmd
        , removeItemCmd
        , askDataCmd
        , DataID
        )

import Data exposing (Data, Object)
import Json.Decode
import Utils exposing (Index)
import Json.Encode
import Platform.Sub


type alias DataID =
    String


port getDataSubPort : (( DataID, Json.Encode.Value ) -> msg) -> Sub msg


getDataSub : (DataID -> Result String Data -> a) -> Sub a
getDataSub msgBuilder =
    let
        objOfJson json =
            Json.Decode.decodeValue Data.dataDecoder json
    in
        getDataSubPort (\( id, json ) -> msgBuilder id (objOfJson json))


getStringSub : (DataID -> String -> a) -> Sub a
getStringSub msgBuilder =
    getDataSub
        (\id res ->
            case res of
                Ok (Data.String s) ->
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


port itemAddedSubPort : (( DataID, Index, Json.Encode.Value ) -> msg) -> Sub msg


itemAddedSub : (DataID -> Index -> Result String Data -> c) -> Sub c
itemAddedSub msgBuilder =
    let
        objOfJson json =
            Json.Decode.decodeValue Data.dataDecoder json
    in
        itemAddedSubPort (\( sp, i, json ) -> msgBuilder sp i (objOfJson json))


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
