module Main exposing (..)

import Html
import SelectableText
import Storage
import Widget exposing (makeTopWidget)


--import SelectableList

import SelectableText


--import NewText

import Widget
import Storage


textBinding : Widget.Binding msg String ()
textBinding =
    { get =
        Storage.getStringSub
            (\( path, s ) ->
                \p ->
                    if path == p then
                        Result.Ok s
                    else
                        Result.Err ()
            )
    , set = \s p -> Storage.setStringCmd ( p, s )
    }


{-|
listBinding : Widget.ListBinding msg ()
listBinding p =
    { itemAdded =
        Storage.itemAddedSub
            (\path ->
                case listSubstract path p of
                    Just [ i ] ->
                        Result.mapError (always ()) (String.toInt i)

                    _ ->
                        Result.Err ()
            )
    , itemRemoved =
        Storage.itemRemovedSub
            (\path ->
                case listSubstract path p of
                    Just [ i ] ->
                        Result.mapError (always ()) (String.toInt i)

                    _ ->
                        Result.Err ()
            )
    , addItem = \i -> Storage.addItemCmd (p ++ [ toString i ])
    , removeItem = \i -> Storage.removeItemCmd (p ++ [ toString i ])
    }
-
-}
main : Program Never SelectableText.Model SelectableText.Msg
main =
    let
        widget =
            SelectableText.createWidget textBinding
    in
        Html.program <|
            makeTopWidget
                { init = widget.init
                , update = widget.update
                , view = widget.view
                , subscriptions = widget.subscriptions
                }



--  SelectableList.createListWidget listBinding [] NewText.widget (SelectableText.createWidget textBinding))
{--main : Program Never (SelectableList.Model SelectableText.Model) (SelectableList.Msg NewText.Msg SelectableText.Msg)
main =
    Html.program (SelectableList.createListWidget NewText.widget SelectableText.widget)
        Html.program--}
