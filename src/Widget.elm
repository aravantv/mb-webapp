module Widget exposing (..)

import Html exposing (Html)
import Task


-- Row types are used only to compose new types in a modular fashion,
-- *not* to make functions accept abstract structures:
-- just use wrappers if you do not have the precise concrete type


type alias Widget model msg =
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }


type alias Binding msg serializedType err =
    { get : Sub (Result err serializedType)
    , set : serializedType -> Cmd msg
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


type alias IListBinding msg itemSerializedType err base =
    { base
        | itemAdded : Sub (Result err ( Index, itemSerializedType ))
        , itemRemoved : Sub (Result err Index)
        , addItem : Index -> itemSerializedType -> Cmd msg
        , removeItem : Index -> Cmd msg
    }


type alias ListBinding msg itemSerializedType err =
    IListBinding msg itemSerializedType err (Binding msg (List itemSerializedType) err)


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
