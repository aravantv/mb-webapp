module Data exposing (..)

import Dict exposing (Dict)
import Json.Decode as Dec exposing (Decoder)
import Json.Encode
import JsonParameters exposing (boolTypeID, intTypeID, sanitizeName, stringTypeID, typeFieldName, unsanitizeName, valueFieldName)


type alias Object =
    Dict String AttributeValue


jsonOfObject : Object -> Json.Encode.Value
jsonOfObject obj =
    let
        attrs =
            List.map (\( name, value ) -> ( sanitizeName name, jsonOfAttributeValue value )) (Dict.toList obj)
    in
        Json.Encode.object attrs


objectDecoder : Decoder Object
objectDecoder =
    Dec.dict <| Dec.lazy (\_ -> attributeDecoder)


type Data
    = String String
    | Int Int
    | Bool Bool
    | ObjectRef Object


getString : Data -> Maybe String
getString d =
    case d of
        String s ->
            Just s

        _ ->
            Nothing


isString : Data -> Bool
isString d =
    getString d /= Nothing


getInt : Data -> Maybe Int
getInt d =
    case d of
        Int n ->
            Just n

        _ ->
            Nothing


isInt : Data -> Bool
isInt d =
    getInt d /= Nothing


getBool : Data -> Maybe Bool
getBool d =
    case d of
        Bool b ->
            Just b

        _ ->
            Nothing


isBool : Data -> Bool
isBool d =
    getBool d /= Nothing


getObject : Data -> Maybe Object
getObject d =
    case d of
        ObjectRef o ->
            Just o

        _ ->
            Nothing


isObject : Data -> Bool
isObject d =
    getObject d /= Nothing


jsonOfData : Data -> Json.Encode.Value
jsonOfData d =
    case d of
        String s ->
            Json.Encode.string s

        Int n ->
            Json.Encode.int n

        Bool b ->
            Json.Encode.bool b

        ObjectRef obj ->
            jsonOfObject obj


dataDecoder : Decoder Data
dataDecoder =
    Dec.oneOf
        [ Dec.map String Dec.string
        , Dec.map Int Dec.int
        , Dec.map Bool Dec.bool
        , Dec.map ObjectRef <| Dec.lazy (\_ -> objectDecoder)
        ]


type AttributeValue
    = SingleData (Maybe Data)
    | MultipleData (List Data)


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

            MultipleData l ->
                encodeList jsonOfData l


attributeDecoder : Dec.Decoder AttributeValue
attributeDecoder =
    Dec.oneOf
        [ Dec.map SingleData <| Dec.nullable <| Dec.lazy (\_ -> dataDecoder)
        , Dec.map MultipleData <| Dec.list <| Dec.lazy (\_ -> dataDecoder)
        ]
