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


type alias Widget model msg =
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }


wrapUpdateWithCmd : (msg -> model -> model) -> msg -> model -> ( model, Cmd msg )
wrapUpdateWithCmd update =
    \msg model -> update msg model ! []


wrapModelWithCmd : model -> ( model, Cmd msg )
wrapModelWithCmd model =
    ( model, Cmd.none )


emptySubscription : model -> Sub msg
emptySubscription _ =
    Sub.none
