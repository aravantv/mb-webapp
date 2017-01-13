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


insert : List t -> t -> Int -> List t
insert l x i =
    case l of
        [] ->
            [ x ]

        y :: ys ->
            if i == 0 then
                x :: l
            else
                y :: insert ys x (i - 1)
