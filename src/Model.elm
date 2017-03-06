module Model exposing (..)

import Dict exposing (Dict)
import Json.Decode as Dec exposing (Decoder)
import Json.Encode
import MetaModel exposing (AttributeDescription, AttributeType, ClassDictionary, ClassRef, classRefDecoder, matchesClass, stringOfClassRef)


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


objectDecoder : ClassDictionary -> MetaModel.ClassDef -> Decoder Object
objectDecoder classDict classDef =
    let
        attrsDecoder =
            attributesDecoder classDict classDef.class.attributes
    in
        Dec.map2 (\ref attrs -> { classRef = ref, attributes = attrs }) (classRefFieldDecoder classDef) attrsDecoder


attributesDecoder : ClassDictionary -> Dict String AttributeDescription -> Decoder (Dict String AttributeValue)
attributesDecoder classDict attrs =
    let
        accumulateFieldDecoder requestedAttrName requestedAttrDesc decoderAcc =
            let
                fieldDecoder =
                    Dec.field (sanitizeAttributeName requestedAttrName) (attributeTypeDecoder classDict requestedAttrDesc.type_)
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
    = String String
    | Int Int
    | Bool Bool
    | ObjectRef Object


jsonOfAttributeValue : AttributeValue -> Json.Encode.Value
jsonOfAttributeValue attributeValue =
    case attributeValue of
        String s ->
            Json.Encode.string s

        Int n ->
            Json.Encode.int n

        Bool b ->
            Json.Encode.bool b

        ObjectRef obj ->
            jsonOfObject obj


attributeTypeDecoder : ClassDictionary -> AttributeType -> Dec.Decoder AttributeValue
attributeTypeDecoder classDict ty =
    case ty of
        MetaModel.String ->
            Dec.map String Dec.string

        MetaModel.Int ->
            Dec.map Int Dec.int

        MetaModel.Bool ->
            Dec.map Bool Dec.bool

        MetaModel.ClassRef ref ->
            case MetaModel.classDefOfClassRef classDict ref of
                Just classDef ->
                    Dec.map ObjectRef (objectDecoder classDict classDef)

                Nothing ->
                    Dec.fail <| "Class " ++ stringOfClassRef ref ++ " not found"
