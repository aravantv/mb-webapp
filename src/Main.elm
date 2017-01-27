module Main exposing (..)

import Html
import NewText
import SelectableList
import SelectableText exposing (modelFromString)
import LocalStorage
import Utils exposing (..)
import Widget exposing (Index, makeTopWidget)


textBinding : Widget.Binding msg String ()
textBinding =
    { get =
        \p ->
            Storage.getStringSub
                (\( path, s ) ->
                    if path == p then
                        Result.Ok s
                    else
                        Result.Err ()
                )
    , set = \p s -> Storage.setStringCmd ( p, s )
    }


listBinding : Widget.ListBinding msg ()
listBinding =
    { itemAdded =
        \p ->
            Storage.itemAddedSub
                (\path ->
                    case listSubstract path p of
                        Just [ Widget.Index i ] ->
                            Result.Ok i

                        _ ->
                            Result.Err ()
                )
    , itemRemoved =
        \p ->
            Storage.itemRemovedSub
                (\path ->
                    case listSubstract path p of
                        Just [ Widget.Index i ] ->
                            Result.Ok i

                        _ ->
                            Result.Err ()
                )
    , addItem = \p i -> Storage.addItemCmd (Index i :: p)
    , removeItem = \p i -> Storage.removeItemCmd (Index i :: p)
    }


main : Program Never (SelectableList.Model NewText.Model SelectableText.Model) (SelectableList.Msg NewText.Msg SelectableText.Msg SelectableText.Model)
main =
    let
        widget =
            --SelectableText.createWidget textBinding
            SelectableList.createListWidget listBinding NewText.widget (SelectableText.createWidget textBinding) modelFromString
    in
        Html.program <|
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
