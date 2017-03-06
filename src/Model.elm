module Model exposing (..)

import Dict exposing (Dict)
import Json.Decode as Dec exposing (Decoder)
import Json.Encode
import MetaModel exposing (AttributeDescription, AttributeType, MetaModel, ClassRef, Multiplicity, classRefDecoder, matchesClass, stringOfClassRef)


type alias Object =
    { classRef : MetaModel.ClassRef
    , attributes : Dict String AttributeValue
    }


sanitizeAttributeName : String -> String
sanitizeAttributeName name =
    if name == classFieldName then
        name ++ "_"
    else
        name


classFieldName : String
classFieldName =
    "classReference"


jsonOfObject : Object -> Json.Encode.Value
jsonOfObject obj =
    let
        attrs =
            List.map (\( name, value ) -> ( sanitizeAttributeName name, jsonOfAttributeValue value )) (Dict.toList obj.attributes)
    in
        Json.Encode.object <| ( classFieldName, MetaModel.jsonOfClassRef obj.classRef ) :: attrs


objectDecoder : MetaModel -> MetaModel.ClassRef -> Decoder Object
objectDecoder mm classRef =
    case MetaModel.classDefOfClassRef mm classRef of
        Just classDef ->
            let
                attrsDecoder =
                    attributesDecoder mm classDef.class.attributes
            in
                Dec.map2 (\ref attrs -> { classRef = ref, attributes = attrs }) (classRefFieldDecoder classDef) attrsDecoder

        Nothing ->
            Dec.fail <| "Class " ++ MetaModel.stringOfClassRef classRef ++ " unknown"


attributesDecoder : MetaModel -> Dict String AttributeDescription -> Decoder (Dict String AttributeValue)
attributesDecoder mm attrs =
    let
        accumulateFieldDecoder requestedAttrName requestedAttrDesc decoderAcc =
            let
                fieldDecoder =
                    Dec.field (sanitizeAttributeName requestedAttrName) (attributeDescDecoder mm requestedAttrDesc)
            in
                Dec.map2 (\dict attrValue -> Dict.insert requestedAttrName attrValue dict) decoderAcc fieldDecoder
    in
        Dict.foldl accumulateFieldDecoder (Dec.succeed Dict.empty) attrs


classRefFieldDecoder : MetaModel.ClassDef -> Dec.Decoder ClassRef
classRefFieldDecoder class =
    let
        filtersClass classRef =
            if matchesClass classRef class then
                Dec.succeed classRef
            else
                Dec.fail (MetaModel.stringOfClassRef classRef)
    in
        Dec.field classFieldName classRefDecoder |> Dec.andThen filtersClass


type AttributeValue
    = String (Maybe String)
    | Int (Maybe Int)
    | Bool (Maybe Bool)
    | ObjectRef (Maybe Object)
    | StringList (List String)
    | IntList (List Int)
    | BoolList (List Bool)
    | ObjectRefList (List Object)


jsonOfAttributeValue : AttributeValue -> Json.Encode.Value
jsonOfAttributeValue attributeValue =
    let
        encodeList atomicEncoder =
            Json.Encode.list << List.map atomicEncoder
    in
        case attributeValue of
            String Nothing ->
                Json.Encode.null

            Int Nothing ->
                Json.Encode.null

            Bool Nothing ->
                Json.Encode.null

            ObjectRef Nothing ->
                Json.Encode.null

            String (Just s) ->
                Json.Encode.string s

            Int (Just n) ->
                Json.Encode.int n

            Bool (Just b) ->
                Json.Encode.bool b

            ObjectRef (Just obj) ->
                jsonOfObject obj

            StringList l ->
                encodeList Json.Encode.string l

            IntList l ->
                encodeList Json.Encode.int l

            BoolList l ->
                encodeList Json.Encode.bool l

            ObjectRefList l ->
                encodeList jsonOfObject l


attributeDescDecoder : MetaModel -> AttributeDescription -> Dec.Decoder AttributeValue
attributeDescDecoder mm desc =
    case desc.type_ of
        MetaModel.String ->
            case desc.multiplicity of
                MetaModel.Single ->
                    Dec.map String <| Dec.maybe Dec.string

                MetaModel.Multiple ->
                    Dec.map StringList <| Dec.list Dec.string

        MetaModel.Int ->
            case desc.multiplicity of
                MetaModel.Single ->
                    Dec.map Int <| Dec.maybe Dec.int

                MetaModel.Multiple ->
                    Dec.map IntList <| Dec.list Dec.int

        MetaModel.Bool ->
            case desc.multiplicity of
                MetaModel.Single ->
                    Dec.map Bool <| Dec.maybe Dec.bool

                MetaModel.Multiple ->
                    Dec.map BoolList <| Dec.list Dec.bool

        MetaModel.ClassRef ref ->
            case desc.multiplicity of
                MetaModel.Single ->
                    Dec.map ObjectRef <| Dec.maybe (objectDecoder mm ref)

                MetaModel.Multiple ->
                    Dec.map ObjectRefList <| Dec.list (objectDecoder mm ref)
