module Widget exposing (..)

import Html exposing (Html)
import Task


-- Row types are used only to compose new types in a modular fashion,
-- *not* to make functions accept abstract structures:
-- just use wrappers if you do not have the precise concrete type


type alias Path =
    List String


{-| note: all effects are functions taking a path as parameter, but the in the top-widget,
the one given to Html.program, these are not present anymore
-}
type alias Widget model msg =
    { init : Path -> ( model, Cmd msg )
    , update : Path -> msg -> model -> ( model, Cmd msg )
    , subscriptions : Path -> model -> Sub msg
    , view : model -> Html msg
    }


type alias TopWidget model msg =
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }


makeTopWidget : Widget model msg -> TopWidget model msg
makeTopWidget widget =
    { init = widget.init []
    , update = widget.update []
    , subscriptions = widget.subscriptions []
    , view = widget.view
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


type alias Binding msg serializedType err =
    { get : Path -> Sub (Result err serializedType)
    , set : Path -> serializedType -> Cmd msg
    }


type alias ListBinding msg err =
    { itemAdded : Path -> Sub (Result err Index)
    , itemRemoved : Path -> Sub (Result err Index)
    , addItem : Path -> Index -> Cmd msg
    , removeItem : Path -> Index -> Cmd msg
    }


type alias Factory fromTy toTy =
    fromTy -> toTy


wrapUpdateWithCmd : (msg -> model -> model) -> msg -> model -> ( model, Cmd msg )
wrapUpdateWithCmd update =
    \msg model -> update msg model ! []


wrapWithNoCmd : a -> ( a, Cmd msg )
wrapWithNoCmd x =
    ( x, Cmd.none )


cmdOfMsg : msg -> Cmd msg
cmdOfMsg msg =
    Task.perform identity (Task.succeed msg)


emptySubscription : model -> Sub msg
emptySubscription _ =
    Sub.none
