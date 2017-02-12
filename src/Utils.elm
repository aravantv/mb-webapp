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


{-| substract [x,y,z,t] [x,y] === [z,t]
-}
listSubstract : List t -> List t -> Maybe (List t)
listSubstract l1 l2 =
    case ( l1, l2 ) of
        ( [], [] ) ->
            Just []

        ( [], _ :: _ ) ->
            Nothing

        ( x :: xs, [] ) ->
            Just (x :: xs)

        ( x :: xs, y :: ys ) ->
            if (x == y) then
                listSubstract xs ys
            else
                Nothing


get : List t -> Int -> Maybe t
get l i =
    List.head (List.drop i l)


resultFullMap : (o1 -> o2) -> (e1 -> e2) -> Result e1 o1 -> Result e2 o2
resultFullMap fOk fErr =
    Result.map fOk << Result.mapError fErr
