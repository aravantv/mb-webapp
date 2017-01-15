module Main exposing (..)

import Html


-- import SelectableList

import SelectableText
import Widget exposing (toConcreteWidget, toConcreteWidgetWithFlags)
import Storage exposing (..)


textBinding : Widget.Binding msg String ()
textBinding =
    { get =
        getStringSub
            (\( path, s ) ->
                if path == [] then
                    Result.Ok s
                else
                    Result.Err ()
            )
    , set = \s -> setStringCmd ( [], s )
    }


main : Program Never SelectableText.Model SelectableText.Msg
main =
    Html.program (toConcreteWidget <| SelectableText.createWidget textBinding)



{--main : Program Never (SelectableList.Model SelectableText.Model) (SelectableList.Msg NewText.Msg SelectableText.Msg)
main =
    Html.program (SelectableList.createListWidget NewText.widget SelectableText.widget)
        Html.program--}
