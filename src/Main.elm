module Main exposing (..)

import Binding exposing (textBinding)
import GroupWidget exposing (..)
import CircleWidget
import Label
import NewText
import SelectableList
import SelectableText
import Text
import TimeTravel.Html as TimeTravel
import Widget exposing (ISelectable, Index, makeTopWidget)


listExampleWidget =
    GroupWidget.createGroupWidget
        { divOrSpan = Div
        , wrappedWidget1 = Label.createWidget "List of stuff:"
        , pathAdapter1 = identity
        , wrappedWidget2 =
            SelectableList.createListWidget
                { binding = Binding.listBinding
                , newItemWidget = NewText.widget
                , itemWidget = SelectableText.createWidget Binding.textBinding
                , converter = identity
                }
        , pathAdapter2 = identity
        }


formExampleWidget =
    GroupWidget.createGroupWidget
        { divOrSpan = Div
        , wrappedWidget1 =
            GroupWidget.createGroupWidget
                { divOrSpan = Span
                , wrappedWidget1 =
                    CircleWidget.createCircleWidget
                        { wrappedWidget = Text.createWidget Binding.textBinding, pathAdapter = identity }
                , pathAdapter1 = identity
                , wrappedWidget2 = Text.createWidget Binding.textBinding
                , pathAdapter2 = identity
                }
        , pathAdapter1 = identity
        , wrappedWidget2 =
            GroupWidget.createGroupWidget
                { divOrSpan = Span
                , wrappedWidget1 = Label.createWidget "Number+2:"
                , pathAdapter1 = identity
                , wrappedWidget2 = Text.createWidget (Binding.intToStringTransformer Binding.plus2Binding textBinding)
                , pathAdapter2 = identity
                }
        , pathAdapter2 = identity
        }


main =
    let
        widget =
            formExampleWidget
    in
        TimeTravel.program <|
            makeTopWidget
                { initModel = widget.initModel
                , initMsg = widget.initMsg
                , update = widget.update
                , view = widget.view
                , subscriptions = widget.subscriptions
                }
