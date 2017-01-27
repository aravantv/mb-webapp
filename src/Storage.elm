port module Storage exposing (..)

import Platform.Sub


{-| Paths are provided as a list of string: the root is the *last* element.
-}
type alias Path =
    List String


fieldOfIndex : Int -> String
fieldOfIndex i =
    toString i


type alias JsonString =
    String


port getStringSub : (( Path, JsonString ) -> msg) -> Sub msg


port setStringCmd : ( Path, String ) -> Cmd msg


port itemAddedSub : (Path -> msg) -> Sub msg


port itemRemovedSub : (Path -> msg) -> Sub msg


{-| addItemCmd literally adds an item: it inserts an element at the given path but does not fill it in with any value!
-}
port addItemCmd : Path -> Cmd msg


port removeItemCmd : Path -> Cmd msg
