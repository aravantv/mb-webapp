module Main exposing (..)

import Html
import SelectableText
import Storage
import Widget


--import SelectableList

import SelectableText


--import NewText

import Widget
import Storage


textBinding : Widget.Binding msg String ()
textBinding p =
    { get =
        -- TODO for now I have to return a Result type. In the future, take inspiration from WebSockets instead
        Storage.getStringSub
            (\( path, s ) ->
                if path == p then
                    Result.Ok s
                else
                    Result.Err ()
            )
    , set = \s -> Storage.setStringCmd ( p, s )
    }



{--listBinding : Storage.Path -> Widget.ListBinding msg ()
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
--}


main : Program Never SelectableText.Model SelectableText.Msg
main =
    let
        widget =
            SelectableText.createWidget [] textBinding
    in
        Html.program
            { init = widget.init
            , update = widget.update
            , view = widget.view
            , subscriptions = widget.subscriptions
            }



--  SelectableList.createListWidget (listBinding []) NewText.widget (SelectableText.createWidget (textBinding [ "0" ]))
{--main : Program Never (SelectableList.Model SelectableText.Model) (SelectableList.Msg NewText.Msg SelectableText.Msg)
main =
    Html.program (SelectableList.createListWidget NewText.widget SelectableText.widget)
        Html.program--}
