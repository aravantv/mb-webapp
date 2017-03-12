module Main exposing (..)

import Binding exposing (textBinding)
import CircleWidget
import GroupWidget exposing (..)
import Label
import MetaModel exposing (MetaModel, Multiplicity, RootedMetaModel)
import Model
import NewText
import SelectableList
import SelectableText
import TimeTravel.Html as TimeTravel
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
                , selector1 = identity
                , wrappedWidget2 =
                    SelectableList.createWidget
                        { binding = Binding.listBinding metamodel.metamodel
                        , newItemWidget = NewText.widget
                        , itemWidget = SelectableText.createSelectableWidget Binding.textBinding
                        , factory = Model.String
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
                        { binding = Binding.listBinding metamodel.metamodel
                        , newItemWidget = NewText.widget
                        , itemWidget = SelectableText.createSelectableWidget Binding.textBinding
                        , factory = Model.String
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
            listExampleWidget
    in
        TimeTravel.program <|
            makeTopWidget
                { initModel = widget.initModel
                , initMsg = widget.initMsg
                , update = widget.update
                , view = widget.view
                , subscriptions = widget.subscriptions
                }
                -- ICI donner le metamodel aussi et celui-ci serait ensuite transmis aux sous-bindings?
                [ MetaModel.Field "todos" ]
