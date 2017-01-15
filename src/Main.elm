module Main exposing (..)

import Html


-- import SelectableList

import SelectableText
import Widget exposing (toConcreteWidget, toConcreteWidgetWithFlags)
import Storage


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


main : Program Never SelectableText.Model SelectableText.Msg
main =
    Html.program (toConcreteWidget <| SelectableText.createWidget (textBinding [ "0" ]))



{--main : Program Never (SelectableList.Model SelectableText.Model) (SelectableList.Msg NewText.Msg SelectableText.Msg)
main =
    Html.program (SelectableList.createListWidget NewText.widget SelectableText.widget)
        Html.program--}
