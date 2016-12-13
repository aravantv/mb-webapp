module Main exposing (..)

import Html
import SelectableList
import SelectableText


main : Program Never (SelectableList.Model SelectableText.Model) (SelectableList.Msg SelectableText.Msg)
main =
    Html.program (SelectableList.createListWidget SelectableText.widget)
