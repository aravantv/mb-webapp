port module Storage exposing (..)

import Platform.Sub
import Json.Encode


type alias Path =
    List String


fieldOfIndex : Int -> String
fieldOfIndex i =
    toString i


type alias JsonString =
    String


port getStringSub : (( Path, JsonString ) -> msg) -> Sub msg


port setStringCmd : ( Path, String ) -> Cmd msg


port itemAddedSub : (( Path, JsonString ) -> msg) -> Sub msg


port itemRemovedSub : (Path -> msg) -> Sub msg


port addItemCmd : ( Path, Json.Encode.Value ) -> Cmd msg


port removeItemCmd : Path -> Cmd msg
