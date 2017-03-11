module Widget exposing (..)

import Html exposing (Html)
import MetaModel exposing (ModelElementIdentifier)
import Model exposing (Model)
import Task


-- Row types are used only to compose new types in a modular fashion,
-- *not* to make functions accept abstract structures:
-- just use wrappers if you do not have the precise concrete type


{-| note: all effects are functions taking a path as parameter, but the in the top-widget,
the one given to Html.program, these are not present anymore
-}
type alias Widget model msg =
    { initModel : model
    , initMsg : Model -> msg
    , update : msg -> model -> ModelElementIdentifier -> ( model, Cmd msg )
    , subscriptions : model -> ModelElementIdentifier -> Sub msg
    , view : model -> Html msg
    }


type alias TopWidget model msg =
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }


makeTopWidget : Widget model msg -> ModelElementIdentifier -> TopWidget model msg
makeTopWidget widget rootId =
    { init = doNothing widget.initModel
    , update = \ms m -> widget.update ms m rootId
    , subscriptions = \m -> widget.subscriptions m rootId
    , view = widget.view
    }


type alias UnboundWidget model msg =
    { initModel : model
    , initMsg : Model -> msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }


makeBoundWidget : UnboundWidget model msg -> Widget model msg
makeBoundWidget widget =
    { initModel = widget.initModel
    , initMsg = widget.initMsg
    , update = \msg model p -> widget.update msg model
    , subscriptions = \model p -> widget.subscriptions model
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
