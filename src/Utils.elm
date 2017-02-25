module Utils exposing (..)

import Char exposing (KeyCode)
import Html exposing (Html)
import Html.Events exposing (keyCode, on)
import Json.Decode exposing (andThen, map2, fail, succeed)


shiftKey : Json.Decode.Decoder Bool
shiftKey =
    Json.Decode.field "shiftKey" Json.Decode.bool


type alias FullKeyCode =
    { keyCode : KeyCode, isShift : Bool }


standardKeyCode : KeyCode -> FullKeyCode
standardKeyCode n =
    { keyCode = n, isShift = False }


shiftCode : FullKeyCode -> FullKeyCode
shiftCode kc =
    { kc | isShift = True }


onKey : String -> List ( FullKeyCode, msg ) -> Html.Attribute msg
onKey s l =
    let
        keyUpDecoder refKc =
            case List.filterMap (\( kc, msg ) -> maybeIf (kc == refKc) msg) l of
                [] ->
                    fail "Key not handled"

                msg :: _ ->
                    succeed msg
    in
        on s (andThen keyUpDecoder (map2 FullKeyCode keyCode shiftKey))


onKeyUp : List ( FullKeyCode, msg ) -> Html.Attribute msg
onKeyUp =
    onKey "keyup"


onKeyDown : List ( FullKeyCode, msg ) -> Html.Attribute msg
onKeyDown =
    onKey "keydown"


tabKey : FullKeyCode
tabKey =
    standardKeyCode 9


enterKey : FullKeyCode
enterKey =
    standardKeyCode 13


escapeKey : FullKeyCode
escapeKey =
    standardKeyCode 27


resultFullMap : (o1 -> o2) -> (e1 -> e2) -> Result e1 o1 -> Result e2 o2
resultFullMap fOk fErr =
    Result.map fOk << Result.mapError fErr


maybeIf : Bool -> a -> Maybe a
maybeIf b v =
    if b then
        Just v
    else
        Nothing
