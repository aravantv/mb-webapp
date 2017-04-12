module Main exposing (..)

import Binding exposing (textBinding)
import CircleWidget
import Data exposing (Data(..))
import DataType exposing (ClassRef, DataType, DataTypeSet, FullDataType, Multiplicity, dataTypeSet)
import GroupWidget exposing (..)
import Label
import NewText
import SelectableList
import SelectableText
import Text
import TimeTravel.Html as TimeTravel
import Widget exposing (ISelectable, Index, makeTopWidget)


-- METAMODEL


metamodel : FullDataType
metamodel =
    { root = ClassRef "MyList"
    , dataTypeSet =
        dataTypeSet
            [ ( "MyList"
              , [ ( "todos", { type_ = DataType.String, isReference = False, multiplicity = DataType.Multiple } ) ]
              )
            ]
    }



-- WIDGET


listExampleWidget =
    GroupWidget.createWidget
        { divOrSpan = Div
        , wrappedWidget1 =
            GroupWidget.createWidget
                { divOrSpan = Div
                , wrappedWidget1 = Label.createWidget "List of stuff:"
                , selector1 = identity
                , wrappedWidget2 =
                    SelectableList.createWidget
                        { binding = Binding.listBinding metamodel.dataTypeSet
                        , newItemWidget = NewText.widget
                        , itemWidget = SelectableText.createSelectableWidget Binding.textBinding
                        , factory = String
                        }
                , selector2 = identity
                }
        , selector1 = identity
        , wrappedWidget2 =
            GroupWidget.createWidget
                { divOrSpan = Div
                , wrappedWidget1 = Label.createWidget "Only numbers:"
                , selector1 = identity
                , wrappedWidget2 =
                    SelectableList.createWidget
                        { binding = Binding.listBinding metamodel.dataTypeSet
                        , newItemWidget = NewText.widget
                        , itemWidget = SelectableText.createSelectableWidget Binding.textBinding
                        , factory = String
                        }
                , selector2 = identity
                }
        , selector2 = identity
        }


formExampleWidget =
    GroupWidget.createWidget
        { divOrSpan = Div
        , wrappedWidget1 =
            GroupWidget.createWidget
                { divOrSpan = Span
                , wrappedWidget1 =
                    CircleWidget.createWidget
                        { wrappedWidget = SelectableText.createWidget Binding.textBinding, selector = identity }
                , selector1 = identity
                , wrappedWidget2 = Text.createWidget Binding.textBinding
                , selector2 = identity
                }
        , selector1 = identity
        , wrappedWidget2 =
            GroupWidget.createWidget
                { divOrSpan = Span
                , wrappedWidget1 = Label.createWidget "Number+2:"
                , selector1 = identity
                , wrappedWidget2 = Text.createWidget (Binding.intToStringTransformer Binding.plus2Binding textBinding)
                , selector2 = identity
                }
        , selector2 = identity
        }


main =
    let
        widget =
            formExampleWidget
    in
        TimeTravel.program <|
            makeTopWidget
                widget
                -- ICI donner le metamodel aussi et celui-ci serait ensuite transmis aux sous-bindings?
                [ DataType.Field "todos" ]
