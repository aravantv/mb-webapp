module Main exposing (..)

import GroupWidget
import LocalStorage
import NewText
import SelectableList
import SelectableText
import TimeTravel.Html as TimeTravel
import ListUtils exposing (..)
import Widget exposing (ISelectable, Index, makeTopWidget)
import Label


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
    , askItemContent = \p i -> LocalStorage.askContentCmd (Index i :: p)
    }


main =
    let
        widget2 =
            SelectableList.createListWidget ( listBinding, NewText.widget, SelectableText.createWidget textBinding, identity )

        widget =
            GroupWidget.createGroupWidget ( Label.createWidget "List of stuff:", identity, widget2, identity )
    in
        TimeTravel.program <|
            makeTopWidget
                { initModel = widget.initModel
                , initMsg = widget.initMsg
                , update = widget.update
                , view = widget.view
                , subscriptions = widget.subscriptions
                }


type SelfReferentialSelectableList newItemModel
    = Self (SelectableList.Model newItemModel (SelfReferentialSelectableList newItemModel))



{--main : Program Never (SelectableList.Model NewText.Model SelectableText.Model) (SelectableList.Msg NewText.Msg SelectableText.Msg)
main =
    Html.program (SelectableList.createListWidget NewText.widget SelectableText.widget)
        Html.program--}
