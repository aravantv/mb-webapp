module Data exposing (..)

import Dict exposing (Dict)
import Json.Decode as Dec exposing (Decoder)
import Json.Encode
import DataType exposing (AttributeDescription, ClassRef, DataTypeSet, DataType, Multiplicity, FullDataType, classRefDecoder, matchesClass, stringOfClassRef)
import JsonParameters exposing (boolTypeID, intTypeID, sanitizeName, stringTypeID, typeFieldName, unsanitizeName, valueFieldName)


type alias Object =
    { classRef : DataType.ClassRef
    , attributes : Dict String AttributeValue
    }


fullDataTypeFactory : FullDataType -> Maybe Data
fullDataTypeFactory fdt =
    case fdt.root of
        DataType.String ->
            Just (String "")

        DataType.Int ->
            Just (Int 0)

        DataType.Bool ->
            Just (Bool False)

        DataType.ClassRef classRef ->
            DataType.classDefOfClassRef fdt.dataTypeSet classRef
                |> Maybe.map
                    (\classDef ->
                        let
                            attrValues =
                                Dict.map (always attributeFactory) classDef.class.attributes
                        in
                            ObjectRef { classRef = classRef, attributes = attrValues }
                    )


jsonOfObject : Object -> Json.Encode.Value
jsonOfObject obj =
    let
        attrs =
            List.map (\( name, value ) -> ( sanitizeName name, jsonOfAttributeValue value )) (Dict.toList obj.attributes)
    in
        Json.Encode.object attrs


attributesDecoder : DataTypeSet -> DataType.ClassRef -> Decoder (Dict String AttributeValue)
attributesDecoder dts classRef =
    case DataType.classDefOfClassRef dts classRef of
        Just classDef ->
            let
                accumulateFieldDecoder requestedAttrName requestedAttrDesc decoderAcc =
                    let
                        fieldDecoder =
                            Dec.field (sanitizeName requestedAttrName) (attributeDecoder dts requestedAttrDesc)
                    in
                        Dec.map2 (\dict attrValue -> Dict.insert requestedAttrName attrValue dict) decoderAcc fieldDecoder
            in
                Dict.foldl accumulateFieldDecoder (Dec.succeed Dict.empty) classDef.class.attributes

        Nothing ->
            Dec.fail <| "Class " ++ DataType.stringOfClassRef classRef ++ " unknown"


objectDecoder : DataTypeSet -> DataType.ClassRef -> Decoder Object
objectDecoder dts classRef =
    Dec.map (\attrs -> { classRef = classRef, attributes = attrs }) <| attributesDecoder dts classRef


type Data
    = String String
    | Int Int
    | Bool Bool
    | ObjectRef Object


typeOfData : Data -> Json.Encode.Value
typeOfData d =
    case d of
        String s ->
            Json.Encode.string stringTypeID

        Int n ->
            Json.Encode.string intTypeID

        Bool b ->
            Json.Encode.string boolTypeID

        ObjectRef obj ->
            let
                json =
                    DataType.jsonOfClassRef obj.classRef
            in
                -- sanitizes the name to ensure no collision with string/bool/int
                case Dec.decodeValue Dec.string json of
                    Ok s ->
                        Json.Encode.string (sanitizeName s)

                    Err _ ->
                        json


jsonOfValue : Data -> Json.Encode.Value
jsonOfValue d =
    case d of
        String s ->
            Json.Encode.string s

        Int n ->
            Json.Encode.int n

        Bool b ->
            Json.Encode.bool b

        ObjectRef obj ->
            jsonOfObject obj


jsonOfData : Data -> Json.Encode.Value
jsonOfData d =
    Json.Encode.object [ ( typeFieldName, typeOfData d ), ( valueFieldName, jsonOfValue d ) ]


dataDecoder : DataTypeSet -> Decoder Data
dataDecoder dts =
    Dec.field typeFieldName Dec.string
        |> Dec.andThen
            (\s ->
                Dec.field valueFieldName <|
                    if s == stringTypeID then
                        Dec.map String Dec.string
                    else if s == intTypeID then
                        Dec.map Int Dec.int
                    else if s == boolTypeID then
                        Dec.map Bool Dec.bool
                    else
                        Dec.map ObjectRef <| objectDecoder dts (unsanitizeName s)
            )


type AttributeValue
    = SingleData (Maybe Data)
    | StringList (List String)
    | IntList (List Int)
    | BoolList (List Bool)
    | ObjectRefList (List Object)


attributeFactory : AttributeDescription -> AttributeValue
attributeFactory attrDesc =
    case attrDesc.multiplicity of
        DataType.Single ->
            SingleData Nothing

        DataType.Multiple ->
            case attrDesc.type_ of
                DataType.String ->
                    StringList []

                DataType.Int ->
                    IntList []

                DataType.Bool ->
                    BoolList []

                DataType.ClassRef _ ->
                    ObjectRefList []


jsonOfAttributeValue : AttributeValue -> Json.Encode.Value
jsonOfAttributeValue attributeValue =
    let
        encodeList atomicEncoder =
            Json.Encode.list << List.map atomicEncoder
    in
        case attributeValue of
            SingleData Nothing ->
                Json.Encode.null

            SingleData (Just d) ->
                jsonOfData d

            StringList l ->
                encodeList Json.Encode.string l

            IntList l ->
                encodeList Json.Encode.int l

            BoolList l ->
                encodeList Json.Encode.bool l

            ObjectRefList l ->
                encodeList jsonOfObject l


attributeDecoder : DataTypeSet -> AttributeDescription -> Dec.Decoder AttributeValue
attributeDecoder dts desc =
    case desc.multiplicity of
        DataType.Single ->
            Dec.map SingleData <| Dec.nullable (dataDecoder dts)

        DataType.Multiple ->
            case desc.type_ of
                DataType.String ->
                    Dec.map StringList <| Dec.list Dec.string

                DataType.Int ->
                    Dec.map IntList <| Dec.list Dec.int

                DataType.Bool ->
                    Dec.map BoolList <| Dec.list Dec.bool

                DataType.ClassRef ref ->
                    Dec.map ObjectRefList <| Dec.list (objectDecoder dts ref)
