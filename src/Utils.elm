module Utils exposing (..)

import Html
import Html.Events exposing (on, keyCode)
import Json.Decode exposing (fail, succeed, andThen)
import Dict exposing (Dict)


-- VIEW


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
