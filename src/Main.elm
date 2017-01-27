module Main exposing (..)

import Html
import NewText
import SelectableList
import SelectableText exposing (modelFromString)
import Storage
import Utils exposing (..)
import Widget exposing (makeTopWidget)


textBinding : Widget.Binding msg String ()
textBinding =
    { get =
        \p ->
            Storage.getStringSub
                (\( path, s ) ->
                    if path == p then
                        Result.Ok s
                    else
                        Result.Err ()
                )
    , set = \p s -> Storage.setStringCmd ( p, s )
    }


listBinding : Widget.ListBinding msg ()
listBinding =
    { itemAdded =
        \p ->
            Storage.itemAddedSub
                (\path ->
                    case listSubstract path p of
                        Just [ i ] ->
                            Result.mapError (always ()) (String.toInt i)

                        _ ->
                            Result.Err ()
                )
    , itemRemoved =
        \p ->
            Storage.itemRemovedSub
                (\path ->
                    case listSubstract path p of
                        Just [ i ] ->
                            Result.mapError (always ()) (String.toInt i)

                        _ ->
                            Result.Err ()
                )
    , addItem = \p i -> Storage.addItemCmd (toString i :: p)
    , removeItem = \p i -> Storage.removeItemCmd (toString i :: p)
    }


main : Program Never (SelectableList.Model NewText.Model SelectableText.Model) (SelectableList.Msg NewText.Msg SelectableText.Msg SelectableText.Model)
main =
    let
        widget =
            --SelectableText.createWidget textBinding
            SelectableList.createListWidget listBinding NewText.widget (SelectableText.createWidget textBinding) modelFromString
    in
        Html.program <|
            makeTopWidget
                { init = widget.init
                , update = widget.update
                , view = widget.view
                , subscriptions = widget.subscriptions
                }



{--main : Program Never (SelectableList.Model NewText.Model SelectableText.Model) (SelectableList.Msg NewText.Msg SelectableText.Msg)
main =
    Html.program (SelectableList.createListWidget NewText.widget SelectableText.widget)
        Html.program--}
