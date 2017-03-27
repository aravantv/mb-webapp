module Widget exposing (..)

import Html exposing (Html)
import MetaModel exposing (ModelElementIdentifier)
import Model exposing (Model)
import Task


{-| note: all effects are functions taking a path as parameter, but the in the top-widget,
the one given to Html.program, these are not present anymore
-}
type alias BoundWidget model msg =
    { initModel : model
    , initMsg : Model -> msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }


type alias Unbound boundWidget =
    ModelElementIdentifier -> boundWidget


type alias Widget model msg =
    Unbound (BoundWidget model msg)


type alias TopWidget model msg =
    ModelElementIdentifier
    -> { init : ( model, Cmd msg )
       , update : msg -> model -> ( model, Cmd msg )
       , subscriptions : model -> Sub msg
       , view : model -> Html msg
       }


makeTopWidget : Widget model msg -> TopWidget model msg
makeTopWidget widget id =
    let
        instantiatedWidget =
            widget id
    in
        { init = doNothing instantiatedWidget.initModel
        , update = instantiatedWidget.update
        , subscriptions = instantiatedWidget.subscriptions
        , view = instantiatedWidget.view
        }



-- Row types are used only to compose new types in a modular fashion,
-- *not* to make functions accept abstract structures:
-- just use wrappers if you do not have the precise concrete type


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


wrapUpdateWithCmd : (msg -> model -> model) -> msg -> model -> ( model, Cmd msg )
wrapUpdateWithCmd update =
    \msg model -> update msg model ! []


doNothing : a -> ( a, Cmd msg )
doNothing x =
    ( x, Cmd.none )


cmdOfMsg : msg -> Cmd msg
cmdOfMsg msg =
    Task.perform identity (Task.succeed msg)


emptySubscription : model -> Sub msg
emptySubscription _ =
    Sub.none


type alias Factory widgetModel =
    widgetModel -> Model
