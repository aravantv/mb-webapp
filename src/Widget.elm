module Widget exposing (..)

import Html exposing (Html)
import Task


-- Row types are used only to compose new types in a modular fashion,
-- *not* to make functions accept abstract structures:
-- just use wrappers if you do not have the precise concrete type


type alias Path =
    List String


type alias Widget model msg =
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }


type alias ISelectable model msg base =
    { base
        | isSelected : model -> Bool
        , selectMsg : msg
        , unselectMsg : msg
    }


type alias IDecision msg base =
    { base | confirmMsg : msg }


type alias Index =
    Int


type alias FixedPathBinding msg serializedType err =
    { get : Sub (Result err serializedType)
    , set : serializedType -> Cmd msg
    }


type alias Binding msg serializedType err =
    Path -> FixedPathBinding msg serializedType err


type alias FixedPathListBinding msg err =
    { itemAdded : Sub (Result err Index)
    , itemRemoved : Sub (Result err Index)
    , addItem : Index -> Cmd msg
    , removeItem : Index -> Cmd msg
    }


type alias ListBinding msg err =
    Path -> FixedPathListBinding msg err


type alias Factory fromTy toTy =
    fromTy -> toTy


wrapUpdateWithCmd : (msg -> model -> model) -> msg -> model -> ( model, Cmd msg )
wrapUpdateWithCmd update =
    \msg model -> update msg model ! []


wrapModelWithCmd : model -> ( model, Cmd msg )
wrapModelWithCmd model =
    ( model, Cmd.none )


cmdOfMsg : msg -> Cmd msg
cmdOfMsg msg =
    Task.perform identity (Task.succeed msg)


emptySubscription : model -> Sub msg
emptySubscription _ =
    Sub.none
