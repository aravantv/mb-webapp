module Main exposing (..)

import Binding exposing (textBinding)
import CircleWidget
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
{--
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
                        { newItemWidget = NewText.createWidget ""
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
                        { binding = (Binding.dataToStringListBinding >> Binding.stringToIntListBinding >> Binding.intToDataListBinding) (Binding.listBinding metamodel.dataTypeSet)
                        , newItemWidget = NewText.widget
                        , itemWidget = SelectableText.createSelectableWidget Binding.textBinding
                        , factory = String
                        }
                , selector2 = identity
                }
        , selector2 = identity
        }
--}
{--
formExampleWidget =
    GroupWidget.createWidget
        { divOrSpan = Div
        , wrappedWidget1 =
            GroupWidget.createWidget
                { divOrSpan = Span
                , wrappedWidget1 =
                    CircleWidget.createWidget
                        { wrappedWidget =
                            SelectableText.widget Binding.textBinding
                        , selector = identity
                        }
                , selector1 = identity
                , wrappedWidget2 = Text.widget Binding.textBinding
                , selector2 = identity
                }
        , selector1 = identity
        , wrappedWidget2 =
            GroupWidget.createWidget
                { divOrSpan = Span
                , wrappedWidget1 = Label.createWidget "Number+2:"
                , selector1 = identity
                , wrappedWidget2 =
                    Text.widget
                    -- (Binding.stringTransformerOfIntTransformer Binding.plus2Binding textBinding)
                , selector2 = identity
                }
        , selector2 = identity
        }

--}


formExampleWidget =
    GroupWidget.createWidget
        { divOrSpan = Div
        , wrappedWidget1 =
            CircleWidget.createWidget <|
                Binding.applyBinding (Binding.textBinding [ DataType.Field "todos" ]) Text.widget
        , wrappedWidget2 = Binding.applyBinding (Binding.textBinding [ DataType.Field "todos" ]) Text.widget
        }


main =
    let
        widget =
            formExampleWidget
    in
        TimeTravel.program (makeTopWidget widget)
