module Main exposing (..)

import Html


-- import SelectableList

import SelectableText
import Widget exposing (toConcreteWidget, toConcreteWidgetWithFlags)
import Storage exposing (..)


textBinding : Int -> Widget.Binding msg String ()
textBinding i =
    { get =
        -- TODO for now I have to return a Maybe type. In the future, take inspiration from WebSockets instead
        getStringSub
            (\( path, s ) ->
                if path == [ fieldOfIndex i ] then
                    Result.Ok s
                else
                    Result.Err ()
            )
    , set = \s -> setStringCmd ( [ fieldOfIndex i ], s )
    }


main : Program Never SelectableText.Model SelectableText.Msg
main =
    Html.program (toConcreteWidget <| SelectableText.createWidget (textBinding 0))



{--main : Program Never (SelectableList.Model SelectableText.Model) (SelectableList.Msg NewText.Msg SelectableText.Msg)
main =
    Html.program (SelectableList.createListWidget NewText.widget SelectableText.widget)
        Html.program--}
