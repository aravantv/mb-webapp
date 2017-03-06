module Model exposing (..)

import Dict exposing (Dict)
import Json.Decode as Dec exposing (Decoder)
import Json.Encode
import MetaModel exposing (AttributeDescription, ClassRef, MetaModel, ModelType, Multiplicity, RootedMetaModel, classRefDecoder, matchesClass, stringOfClassRef)


type alias Object =
    { classRef : MetaModel.ClassRef
    , attributes : Dict String AttributeValue
    }


rootedMetaModelFactory : RootedMetaModel -> Maybe Object
rootedMetaModelFactory rmm =
    MetaModel.classDefOfClassRef rmm.metamodel rmm.root
        |> Maybe.map
            (\classDef ->
                let
                    attrValues =
                        Dict.map (always attributeFactory) classDef.class.attributes
                in
                    { classRef = rmm.root, attributes = attrValues }
            )


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
                    Dec.field (sanitizeAttributeName requestedAttrName) (attributeDecoder mm requestedAttrDesc)
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


type Model
    = String String
    | Int Int
    | Bool Bool
    | ObjectRef Object


jsonOfModel : Model -> Json.Encode.Value
jsonOfModel model =
    case model of
        String s ->
            Json.Encode.string s

        Int n ->
            Json.Encode.int n

        Bool b ->
            Json.Encode.bool b

        ObjectRef obj ->
            jsonOfObject obj


modelDecoder : MetaModel -> MetaModel.ModelType -> Decoder Model
modelDecoder mm ty =
    case ty of
        MetaModel.String ->
            Dec.map String Dec.string

        MetaModel.Int ->
            Dec.map Int Dec.int

        MetaModel.Bool ->
            Dec.map Bool Dec.bool

        MetaModel.ClassRef ref ->
            Dec.map ObjectRef (objectDecoder mm ref)


type AttributeValue
    = SingleModel (Maybe Model)
    | StringList (List String)
    | IntList (List Int)
    | BoolList (List Bool)
    | ObjectRefList (List Object)


attributeFactory : AttributeDescription -> AttributeValue
attributeFactory attrDesc =
    case attrDesc.multiplicity of
        MetaModel.Single ->
            SingleModel Nothing

        MetaModel.Multiple ->
            case attrDesc.type_ of
                MetaModel.String ->
                    StringList []

                MetaModel.Int ->
                    IntList []

                MetaModel.Bool ->
                    BoolList []

                MetaModel.ClassRef _ ->
                    ObjectRefList []


jsonOfAttributeValue : AttributeValue -> Json.Encode.Value
jsonOfAttributeValue attributeValue =
    let
        encodeList atomicEncoder =
            Json.Encode.list << List.map atomicEncoder
    in
        case attributeValue of
            SingleModel Nothing ->
                Json.Encode.null

            SingleModel (Just model) ->
                jsonOfModel model

            StringList l ->
                encodeList Json.Encode.string l

            IntList l ->
                encodeList Json.Encode.int l

            BoolList l ->
                encodeList Json.Encode.bool l

            ObjectRefList l ->
                encodeList jsonOfObject l


attributeDecoder : MetaModel -> AttributeDescription -> Dec.Decoder AttributeValue
attributeDecoder mm desc =
    case desc.multiplicity of
        MetaModel.Single ->
            Dec.map SingleModel <| Dec.nullable (modelDecoder mm desc.type_)

        MetaModel.Multiple ->
            case desc.type_ of
                MetaModel.String ->
                    Dec.map StringList <| Dec.list Dec.string

                MetaModel.Int ->
                    Dec.map IntList <| Dec.list Dec.int

                MetaModel.Bool ->
                    Dec.map BoolList <| Dec.list Dec.bool

                MetaModel.ClassRef ref ->
                    Dec.map ObjectRefList <| Dec.list (objectDecoder mm ref)
