module MetaModel exposing (..)

import Dict exposing (Dict)
import Json.Decode
import Json.Encode


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


type alias RootedMetaModel =
    { root : ClassRef
    , metamodel : MetaModel
    }


type alias Class =
    { attributes : Dict String AttributeDescription }


type alias ClassDef =
    { id : ClassID, class : Class }


type Multiplicity
    = Single
    | Multiple


type alias AttributeDescription =
    { type_ : AttributeType
    , isReference : Bool
    , multiplicity : Multiplicity
    }


type AttributeType
    = String
    | Int
    | Bool
    | ClassRef ClassRef


type alias ClassRef =
    {--Will probably change in the future: uuid? checksum? --}
    String


matchesClass : ClassRef -> ClassDef -> Bool
matchesClass cr c =
    cr == c.id


jsonOfClassRef : ClassRef -> Json.Encode.Value
jsonOfClassRef ref =
    Json.Encode.string ref


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
