module Main exposing (..)

import Binding exposing (applyBinding, intOfStringWrapper, minus2Wrapper, stringOfIntWrapper, textBinding)
import CircleWidget
import CollectionBinding exposing (applyListBinding, listBinding)
import Data
import DataType exposing (ClassRef, DataType, DataTypeSet, FullDataType, Multiplicity, dataTypeSet)
import GroupWidget exposing (..)
import Label
import NewText
import SelectableList
import SelectableText
import TimeTravel.Html as TimeTravel
import Widget exposing (ISelectable, Index, TopWidget, makeTopWidget)


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
                , wrappedWidget2 =
                    SelectableList.createWidget
                        { newItemWidget = NewText.createWidget ""
                        , itemWidget = SelectableText.createSelectableWidget Binding.textBinding
                        , factory = String
                        }
                }
        , wrappedWidget2 =
            GroupWidget.createWidget
                { divOrSpan = Div
                , wrappedWidget1 = Label.createWidget "Only numbers:"
                , wrappedWidget2 =
                    SelectableList.createWidget
                        {
                        -- binding = (Binding.dataToStringListBinding >> Binding.stringToIntListBinding >> Binding.intToDataListBinding) (Binding.listBinding metamodel.dataTypeSet)
                         newItemWidget = NewText.widget
                        , itemWidget = SelectableText.createSelectableWidget Binding.textBinding
                        , factory = String
                        }
                }
        }
--}


listExampleWidget =
    GroupWidget.createWidget
        { divOrSpan = Div
        , wrappedWidget1 = Label.createWidget "List of stuff:"
        , wrappedWidget2 =
            applyListBinding (listBinding "org.vincent.aravantinos.todos") <|
                SelectableList.createWidget
                    { newItemWidget = NewText.createWidget ""
                    , itemWidget = SelectableText.createSelectableWidget (\w id -> applyBinding (textBinding id) w)
                    , factory = Data.String
                    }
        }


formExampleWidget =
    GroupWidget.createWidget
        { divOrSpan = Div
        , wrappedWidget1 =
            CircleWidget.createWidget <|
                GroupWidget.createWidget
                    { divOrSpan = Div
                    , wrappedWidget1 = Label.createWidget "n+2:"
                    , wrappedWidget2 =
                        (applyBinding (textBinding "org.vincent.aravantinos.text")
                            << stringOfIntWrapper
                            << minus2Wrapper
                            << intOfStringWrapper
                        )
                            SelectableText.widget
                    }
        , wrappedWidget2 =
            GroupWidget.createWidget
                { divOrSpan = Div
                , wrappedWidget1 = Label.createWidget "n:"
                , wrappedWidget2 = applyBinding (textBinding "org.vincent.aravantinos.text") SelectableText.widget
                }
        }


main =
    let
        widget =
            listExampleWidget
    in
        TimeTravel.program (makeTopWidget widget)
