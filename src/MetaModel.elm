module MetaModel exposing (..)

import Dict exposing (Dict)
import Json.Decode exposing (decodeString)
import Json.Encode
import ListUtils


type alias ClassID =
    String


type alias MetaModel =
    Dict ClassID Class


metamodel : List ( ClassID, List ( String, AttributeDescription ) ) -> MetaModel
metamodel l =
    let
        lWithDict =
            List.map (\( id, attrs ) -> ( id, { attributes = Dict.fromList attrs } )) l
    in
        Dict.fromList lWithDict


emptyMetamodel : MetaModel
emptyMetamodel =
    Dict.empty


type alias Class =
    { attributes : Dict String AttributeDescription }


type alias ClassDef =
    { id : ClassID, class : Class }


type Multiplicity
    = Single
    | Multiple


type alias AttributeDescription =
    { type_ : ModelType
    , isReference : Bool
    , multiplicity : Multiplicity
    }


type ModelType
    = String
    | Int
    | Bool
    | ClassRef ClassRef


type alias RootedMetaModel =
    { root : ModelType
    , metamodel : MetaModel
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


classDefOfClassRef : MetaModel -> ClassRef -> Maybe ClassDef
classDefOfClassRef mm ref =
    let
        id =
            idOfClassRef ref
    in
        Maybe.map (\c -> { id = id, class = c }) (Dict.get id mm)


classRefDecoder : Json.Decode.Decoder ClassRef
classRefDecoder =
    Json.Decode.string


type GenericField
    = Field String
    | Index Int


stringOfGenericField : GenericField -> String
stringOfGenericField f =
    case f of
        Field s ->
            s

        Index i ->
            toString i


genericFieldOfString : String -> GenericField
genericFieldOfString s =
    case String.toInt s of
        Ok n ->
            Index n

        Err _ ->
            Field s


{-| Paths are provided as a list of string: the root is the *last* element.
-}
type alias Path =
    List GenericField


{-| I see here three possibilities to identify a model element:
* ID for each model element
* path within the model
* checksum
For now we use paths
-}
type alias ModelElementIdentifier =
    Path


getChildIdentifier : ModelElementIdentifier -> String -> ModelElementIdentifier
getChildIdentifier id fieldName =
    Field fieldName :: id


getItemIdentifier : ModelElementIdentifier -> Int -> ModelElementIdentifier
getItemIdentifier id n =
    Index n :: id


isItemOf : ModelElementIdentifier -> ModelElementIdentifier -> Maybe Int
isItemOf candidateModelId id =
    case ListUtils.substract candidateModelId id of
        Just [ Index i ] ->
            Just i

        _ ->
            Nothing


isChildOf : ModelElementIdentifier -> ModelElementIdentifier -> Maybe String
isChildOf candidateModelId id =
    case ListUtils.substract candidateModelId id of
        Just [ Field fieldName ] ->
            Just fieldName

        _ ->
            Nothing


type alias ModelElementSelector =
    ModelElementIdentifier -> ModelElementIdentifier
