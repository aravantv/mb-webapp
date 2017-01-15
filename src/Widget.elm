module Widget exposing (..)

import Html exposing (Html)
import Task


type alias AbstractWidget model msg base =
    { base
        | init : ( model, Cmd msg )
        , update : msg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , view : model -> Html msg
    }


type alias Widget model msg =
    AbstractWidget model msg {}


toConcreteWidget : AbstractWidget model msg base -> Widget model msg
toConcreteWidget widget =
    { init = widget.init, update = widget.update, subscriptions = widget.subscriptions, view = widget.view }


toConcreteWidgetWithFlags :
    AbstractWidget model msg base
    -> { init : flags -> ( model, Cmd msg )
       , update : msg -> model -> ( model, Cmd msg )
       , subscriptions : model -> Sub msg
       , view : model -> Html msg
       }
toConcreteWidgetWithFlags widget =
    { init = \_ -> widget.init, update = widget.update, subscriptions = widget.subscriptions, view = widget.view }


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


type alias ISelectable model msg base =
    { base
        | isSelected : model -> Bool
        , selectMsg : msg
        , unselectMsg : msg
    }


type alias SelectableWidget model msg =
    ISelectable model msg (Widget model msg)


type alias IDecision msg base =
    { base
        | confirmMsg : msg
        , cancelMsg : msg
    }


type alias DecisionWidget model msg =
    IDecision msg (Widget model msg)


type alias Binding msg ty err =
    { get : Sub (Result err ty)
    , set : ty -> Cmd msg
    }


type alias Index =
    Int


type alias ListBinding msg ty err =
    { itemAdded : Sub (Result err ( Index, ty ))
    , itemRemoved : Sub (Result err Index)
    , addItem : Index -> ty -> Cmd msg
    , removeItem : Index -> Cmd msg
    }
