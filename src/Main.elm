module Main exposing (..)

import Binding exposing (textBinding)
import CircleWidget
import GroupWidget exposing (..)
import Html
import Label
import MetaModel exposing (MetaModel, Multiplicity)
import NewText
import SelectableList
import SelectableText
import Text
import Widget exposing (ISelectable, Index, makeTopWidget)


-- METAMODEL


metamodel : RootedMetaModel
metamodel =
    { root = "MyList"
    , metamodel =
        MetaModel.metamodel
            [ ( "MyList"
              , [ ( "todos", { type_ = MetaModel.String, isReference = False, multiplicity = MetaModel.Multiple } ) ]
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
                , pathAdapter1 = identity
                , wrappedWidget2 =
                    SelectableList.createWidget
                        { binding = Binding.listBinding
                        , newItemWidget = NewText.widget
                        , itemWidget = SelectableText.createSelectableWidget Binding.textBinding
                        , converter = identity
                        }
                , pathAdapter2 = identity
                }
        , pathAdapter1 = identity
        , wrappedWidget2 =
            GroupWidget.createWidget
                { divOrSpan = Div
                , wrappedWidget1 = Label.createWidget "Only numbers:"
                , pathAdapter1 = identity
                , wrappedWidget2 =
                    SelectableList.createWidget
                        { binding = Binding.listBinding
                        , newItemWidget = NewText.widget
                        , itemWidget = SelectableText.createSelectableWidget Binding.textBinding
                        , converter = identity
                        }
                , pathAdapter2 = identity
                }
        , pathAdapter2 = identity
        }


formExampleWidget =
    GroupWidget.createWidget
        { divOrSpan = Div
        , wrappedWidget1 =
            GroupWidget.createWidget
                { divOrSpan = Span
                , wrappedWidget1 =
                    CircleWidget.createWidget
                        { wrappedWidget = SelectableText.createWidget Binding.textBinding, pathAdapter = identity }
                , pathAdapter1 = identity
                , wrappedWidget2 = Text.createWidget Binding.textBinding
                , pathAdapter2 = identity
                }
        , pathAdapter1 = identity
        , wrappedWidget2 =
            GroupWidget.createWidget
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
            listExampleWidget
    in
        Html.program <|
            makeTopWidget
                { initModel = widget.initModel
                , initMsg = widget.initMsg
                , update = widget.update
                , view = widget.view
                , subscriptions = widget.subscriptions
                }
