module Utils exposing (..)

import Html exposing (Html)
import Html.Events exposing (on, keyCode)
import Json.Decode exposing (fail, succeed, andThen)
import Dict exposing (Dict)


onKeyUp : List ( Int, msg ) -> Html.Attribute msg
onKeyUp l =
    let
        keyUpDecoder n =
            case Dict.get n <| Dict.fromList l of
                Just msg ->
                    succeed msg

                Nothing ->
                    fail "Key not handled"
    in
        on "keyup" (andThen keyUpDecoder keyCode)


enterKey : number
enterKey =
    13


escapeKey : number
escapeKey =
    27


resultFullMap : (o1 -> o2) -> (e1 -> e2) -> Result e1 o1 -> Result e2 o2
resultFullMap fOk fErr =
    Result.map fOk << Result.mapError fErr
