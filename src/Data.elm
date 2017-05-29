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


objectDecoder : () -> Decoder Object
objectDecoder () =
    Dec.dict attributeDecoder


type Data
    = String String
    | Int Int
    | Bool Bool
    | ObjectRef Object


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
        , Dec.map ObjectRef (objectDecoder ())
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
        [ Dec.map SingleData <| Dec.nullable dataDecoder
        , Dec.map MultipleData <| Dec.list dataDecoder
        ]
