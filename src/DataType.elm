module DataType exposing (..)

import Dict exposing (Dict)
import Json.Decode exposing (decodeString)
import Json.Encode


type alias ClassID =
    String


type alias DataTypeSet =
    Dict ClassID Class


dataTypeSet : List ( ClassID, List ( String, AttributeDescription ) ) -> DataTypeSet
dataTypeSet l =
    let
        lWithDict =
            List.map (\( id, attrs ) -> ( id, { attributes = Dict.fromList attrs } )) l
    in
        Dict.fromList lWithDict


emptyDataTypeSet : DataTypeSet
emptyDataTypeSet =
    Dict.empty


atomicDataType : DataType -> FullDataType
atomicDataType mt =
    { root = mt
    , dataTypeSet = emptyDataTypeSet
    }


stringDataType : FullDataType
stringDataType =
    atomicDataType String


intDataType : FullDataType
intDataType =
    atomicDataType Int


boolDataType : FullDataType
boolDataType =
    atomicDataType Bool


type alias Class =
    { attributes : Dict String AttributeDescription }


type alias ClassDef =
    { id : ClassID, class : Class }


type Multiplicity
    = Single
    | Multiple


type alias AttributeDescription =
    { type_ : DataType
    , isReference : Bool
    , multiplicity : Multiplicity
    }


type DataType
    = String
    | Int
    | Bool
    | ClassRef ClassRef


type alias FullDataType =
    { root : DataType
    , dataTypeSet : DataTypeSet
    }


type alias ClassRef =
    {--Will probably change in the future: uuid? checksum? --}
    String


matchesClass : ClassRef -> ClassDef -> Bool
matchesClass cr c =
    cr == c.id


jsonOfClassRef : ClassRef -> Json.Encode.Value
jsonOfClassRef =
    Json.Encode.string


stringOfClassRef : ClassRef -> String
stringOfClassRef =
    identity


idOfClassRef : ClassRef -> ClassID
idOfClassRef =
    identity


classRefDecoder : Json.Decode.Decoder ClassRef
classRefDecoder =
    Json.Decode.string
