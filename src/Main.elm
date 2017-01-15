module Main exposing (..)

import Html
import Utils exposing (..)


-- import SelectableList

import SelectableText
import Widget exposing (toConcreteWidget, toConcreteWidgetWithFlags)
import Storage
import Json.Encode


textBinding : Storage.Path -> Widget.Binding msg String ()
textBinding p =
    { get =
        -- TODO for now I have to return a Maybe type. In the future, take inspiration from WebSockets instead
        Storage.getStringSub
            (\( path, s ) ->
                if path == p then
                    Result.Ok s
                else
                    Result.Err ()
            )
    , set = \s -> Storage.setStringCmd ( p, s )
    }


listBinding : Storage.Path -> Widget.ListBinding msg String ()
listBinding p =
    { itemAdded =
        Storage.itemAddedSub
            (\( path, v ) ->
                case listSubstract path p of
                    Just [ i ] ->
                        resultFullMap (\n -> ( n, v )) (always ()) (String.toInt i)

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
    , addItem = \i v -> Storage.addItemCmd ( p ++ [ toString i ], (Json.Encode.string v) )
    , removeItem = \i -> Storage.removeItemCmd (p ++ [ toString i ])
    }


main : Program Never SelectableText.Model SelectableText.Msg
main =
    Html.program (toConcreteWidget <| SelectableText.createWidget (textBinding [ "0" ]))



{--main : Program Never (SelectableList.Model SelectableText.Model) (SelectableList.Msg NewText.Msg SelectableText.Msg)
main =
    Html.program (SelectableList.createListWidget NewText.widget SelectableText.widget)
        Html.program--}
