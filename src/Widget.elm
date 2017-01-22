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
    { init : ( model, Path -> Cmd msg )
    , update : msg -> model -> ( model, Path -> Cmd msg )
    , subscriptions : model -> Sub (Path -> msg)
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
    let
        ( model, cmdBuilder ) =
            widget.init

        update msg model =
            let
                ( updatedModel, postUpdateCmdBuilder ) =
                    widget.update msg model
            in
                ( updatedModel, postUpdateCmdBuilder [] )

        subscriptions model =
            Sub.map (\msgBuilder -> msgBuilder []) (widget.subscriptions model)
    in
        { init = ( model, cmdBuilder [] )
        , update = update
        , subscriptions = subscriptions
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
    { get : Sub (Path -> Result err serializedType)
    , set : serializedType -> Path -> Cmd msg
    }


type alias ListBinding msg err =
    { itemAdded : Sub (Path -> Result err Index)
    , itemRemoved : Sub (Path -> Result err Index)
    , addItem : Index -> Path -> Cmd msg
    , removeItem : Index -> Path -> Cmd msg
    }


type alias Factory fromTy toTy =
    fromTy -> toTy


wrapUpdateWithCmd : (msg -> model -> model) -> msg -> model -> ( model, Cmd msg )
wrapUpdateWithCmd update =
    \msg model -> update msg model ! []


wrapWithNoCmd : a -> ( a, b -> Cmd msg )
wrapWithNoCmd x =
    ( x, always Cmd.none )


cmdOfMsg : msg -> Cmd msg
cmdOfMsg msg =
    Task.perform identity (Task.succeed msg)


emptySubscription : model -> Sub msg
emptySubscription _ =
    Sub.none
