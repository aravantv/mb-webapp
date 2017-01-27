module Main exposing (..)

import Html
import NewText
import SelectableList
import SelectableText exposing (modelFromString)
import LocalStorage
import Utils exposing (..)
import Widget exposing (Index, makeTopWidget)
import TimeTravel.Html as TimeTravel


textBinding : Widget.Binding msg String ()
textBinding =
    { get =
        \p ->
            LocalStorage.getStringSub
                (\( path, s ) ->
                    if path == p then
                        Result.Ok s
                    else
                        Result.Err ()
                )
    , set = \p s -> LocalStorage.setStringCmd ( p, s )
    }


listBinding : Widget.ListBinding msg ()
listBinding =
    { itemAdded =
        \p ->
            LocalStorage.itemAddedSub
                (\path ->
                    case listSubstract path p of
                        Just [ Widget.Index i ] ->
                            Result.Ok i

                        _ ->
                            Result.Err ()
                )
    , itemRemoved =
        \p ->
            LocalStorage.itemRemovedSub
                (\path ->
                    case listSubstract path p of
                        Just [ Widget.Index i ] ->
                            Result.Ok i

                        _ ->
                            Result.Err ()
                )
    , addItem = \p i -> LocalStorage.addItemCmd (Index i :: p)
    , removeItem = \p i -> LocalStorage.removeItemCmd (Index i :: p)
    }


main =
    let
        widget =
            --SelectableText.createWidget textBinding
            SelectableList.createListWidget listBinding NewText.widget (SelectableText.createWidget textBinding) modelFromString
    in
        TimeTravel.program <|
            makeTopWidget
                { init = widget.init
                , update = widget.update
                , view = widget.view
                , subscriptions = widget.subscriptions
                }



{--main : Program Never (SelectableList.Model NewText.Model SelectableText.Model) (SelectableList.Msg NewText.Msg SelectableText.Msg)
main =
    Html.program (SelectableList.createListWidget NewText.widget SelectableText.widget)
        Html.program--}
