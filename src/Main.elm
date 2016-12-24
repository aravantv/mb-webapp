port module Main exposing (..)

import Html


{--import SelectableList
import NewText--}

import SelectableText
import Widget exposing (toConcreteWidget, toConcreteWidgetWithFlags)


type alias DataModel =
    { tasks : List String }



{--main : Program Never (SelectableList.Model SelectableText.Model) (SelectableList.Msg NewText.Msg SelectableText.Msg)
main =
    Html.program (SelectableList.createListWidget NewText.widget SelectableText.widget)
        Html.program--}


port getValue : (String -> msg) -> Sub msg


port setValue : String -> Cmd msg


textBinding : Widget.Binding msg String
textBinding =
    { get = getValue identity
    , set = setValue
    }


main : Program Never SelectableText.Model SelectableText.Msg
main =
    Html.program (toConcreteWidget <| SelectableText.createWidget textBinding)
