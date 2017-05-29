module Model exposing (..)

import Dict exposing (Dict)
import Json.Decode as Dec exposing (Decoder)
import Json.Encode
import MetaModel exposing (AttributeDescription, ClassRef, MetaModel, ModelType, Multiplicity, RootedMetaModel, classRefDecoder, matchesClass, stringOfClassRef)
import ModelJsonParameters exposing (boolTypeID, intTypeID, sanitizeName, stringTypeID, typeFieldName, unsanitizeName, valueFieldName)


type alias Object =
    { classRef : MetaModel.ClassRef
    , attributes : Dict String AttributeValue
    }


rootedMetaModelFactory : RootedMetaModel -> Maybe Model
rootedMetaModelFactory rmm =
    case rmm.root of
        MetaModel.String ->
            Just (String "")

        MetaModel.Int ->
            Just (Int 0)

        MetaModel.Bool ->
            Just (Bool False)

        MetaModel.ClassRef classRef ->
            MetaModel.classDefOfClassRef rmm.metamodel classRef
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


attributesDecoder : MetaModel -> MetaModel.ClassRef -> Decoder (Dict String AttributeValue)
attributesDecoder mm classRef =
    case MetaModel.classDefOfClassRef mm classRef of
        Just classDef ->
            let
                accumulateFieldDecoder requestedAttrName requestedAttrDesc decoderAcc =
                    let
                        fieldDecoder =
                            Dec.field (sanitizeName requestedAttrName) (attributeDecoder mm requestedAttrDesc)
                    in
                        Dec.map2 (\dict attrValue -> Dict.insert requestedAttrName attrValue dict) decoderAcc fieldDecoder
            in
                Dict.foldl accumulateFieldDecoder (Dec.succeed Dict.empty) classDef.class.attributes

        Nothing ->
            Dec.fail <| "Class " ++ MetaModel.stringOfClassRef classRef ++ " unknown"


objectDecoder : MetaModel -> MetaModel.ClassRef -> Decoder Object
objectDecoder mm classRef =
    Dec.map (\attrs -> { classRef = classRef, attributes = attrs }) <| attributesDecoder mm classRef


type Model
    = String String
    | Int Int
    | Bool Bool
    | ObjectRef Object


typeOfModel : Model -> Json.Encode.Value
typeOfModel model =
    case model of
        String s ->
            Json.Encode.string stringTypeID

        Int n ->
            Json.Encode.string intTypeID

        Bool b ->
            Json.Encode.string boolTypeID

        ObjectRef obj ->
            let
                json =
                    MetaModel.jsonOfClassRef obj.classRef
            in
                -- sanitizes the name to ensure no collision with string/bool/int
                case Dec.decodeValue Dec.string json of
                    Ok s ->
                        Json.Encode.string (sanitizeName s)

                    Err _ ->
                        json


jsonOfValue : Model -> Json.Encode.Value
jsonOfValue model =
    case model of
        String s ->
            Json.Encode.string s

        Int n ->
            Json.Encode.int n

        Bool b ->
            Json.Encode.bool b

        ObjectRef obj ->
            jsonOfObject obj


jsonOfModel : Model -> Json.Encode.Value
jsonOfModel model =
    Json.Encode.object [ ( typeFieldName, typeOfModel model ), ( valueFieldName, jsonOfValue model ) ]


modelDecoder : MetaModel -> Decoder Model
modelDecoder mm =
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
                        Dec.map ObjectRef <| objectDecoder mm (unsanitizeName s)
            )


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
            Dec.map SingleModel <| Dec.nullable (modelDecoder mm)

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
