module SelectableList exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Utils exposing (..)
import Widget exposing (Factory, IDecision, ISelectable, Index, ListBinding, TopWidget, Widget, wrapWithNoCmd)


type alias ItemWidget model msg =
    ISelectable model msg (Widget model msg)


type alias NewItemWidget model msg =
    IDecision msg (TopWidget model msg)


createListWidget :
    ListBinding (Msg newItemMsg itemMsg itemModel) err
    -> NewItemWidget newItemModel newItemMsg
    -> ItemWidget itemModel itemMsg
    -> Factory newItemModel itemModel itemMsg
    -> Widget (Model newItemModel itemModel) (Msg newItemMsg itemMsg itemModel)
createListWidget binding newItemWidget itemWidget factory =
    { init = init newItemWidget
    , update = update binding factory newItemWidget itemWidget
    , subscriptions = subscriptions binding itemWidget
    , view = view newItemWidget itemWidget
    }



-- MODEL


type alias Model newItemModel itemModel =
    { itemToAdd : newItemModel, contents : List itemModel }


init :
    NewItemWidget newItemModel newItemMsg
    -> Widget.Path
    -> ( Model newItemModel itemModel, Cmd (Msg newItemMsg itemMsg itemModel) )
init newItemWidget p =
    let
        ( itemToAdd, cmdInit ) =
            newItemWidget.init
    in
        ( { itemToAdd = itemToAdd, contents = [] }, Cmd.map UINewItemMsg cmdInit )



-- UPDATE


type Msg newItemMsg itemMsg itemModel
    = UINewItemMsg newItemMsg
    | ItemMsg Index itemMsg
    | UIRemove Index
    | ModelAddedItem Index
    | ModelRemovedItem Index
    | NoOp


update :
    ListBinding (Msg newItemMsg itemMsg itemModel) err
    -> Factory newItemModel itemModel itemMsg
    -> NewItemWidget newItemModel newItemMsg
    -> ItemWidget itemModel itemMsg
    -> Msg newItemMsg itemMsg itemModel
    -> Model newItemModel itemModel
    -> Widget.Path
    -> ( Model newItemModel itemModel, Cmd (Msg newItemMsg itemMsg itemModel) )
update binding factory newItemWidget itemWidget msg model p =
    case msg of
        UINewItemMsg subMsg ->
            if subMsg == newItemWidget.confirmMsg then
                addItem binding factory newItemWidget itemWidget model p
            else
                let
                    ( updatedItemToAdd, cmd ) =
                        newItemWidget.update subMsg model.itemToAdd
                in
                    ( { model | itemToAdd = updatedItemToAdd }, Cmd.map UINewItemMsg cmd )

        ItemMsg i subMsg ->
            propagateMsgToWidget itemWidget model i subMsg p

        UIRemove i ->
            ( modelWithRemovedItem i model, binding.removeItem p i )

        ModelAddedItem i ->
            let
                {--| here: have an empty model? --}
                ( m, _ ) =
                    itemWidget.init p
            in
                ( { model | contents = insert model.contents m i }, binding.getItemContent p i )

        ModelRemovedItem i ->
            wrapWithNoCmd (modelWithRemovedItem i model)

        NoOp ->
            wrapWithNoCmd model


addItem :
    ListBinding (Msg newItemMsg itemMsg itemModel) err
    -> Factory newItemModel itemModel itemMsg
    -> NewItemWidget newItemModel newItemMsg
    -> ItemWidget itemModel itemMsg
    -> Model newItemModel itemModel
    -> Widget.Path
    -> ( Model newItemModel itemModel, Cmd (Msg newItemMsg itemMsg itemModel) )
addItem binding factory newItemWidget itemWidget model p =
    let
        ( newItem, newItemCmd ) =
            factory model.itemToAdd

        ( newNewItemToAdd, newNewItemWidgetCmd ) =
            newItemWidget.init

        cmd =
            Cmd.batch
                [ binding.addItem p 0
                , Cmd.map UINewItemMsg newNewItemWidgetCmd
                , Cmd.map (ItemMsg 0) newItemCmd
                ]

        modelWithItemAdded =
            { itemToAdd = newNewItemToAdd, contents = newItem :: model.contents }
    in
        ( modelWithItemAdded, cmd )


propagateMsgToWidget :
    ItemWidget itemModel itemMsg
    -> Model newItemModel itemModel
    -> Index
    -> itemMsg
    -> Widget.Path
    -> ( Model newItemModel itemModel, Cmd (Msg newItemMsg itemMsg itemModel) )
propagateMsgToWidget widget model i msg p =
    let
        updatedWidgetList =
            List.indexedMap (updateWidget widget i msg p) model.contents

        ( contentsWithWidgetUpdated, updateCmds ) =
            List.unzip updatedWidgetList

        updateCmd =
            Cmd.batch <| List.map (Cmd.map (ItemMsg i)) updateCmds
    in
        if msg /= widget.selectMsg then
            ( { model | contents = contentsWithWidgetUpdated }, updateCmd )
        else
            let
                unselectPreviouslySelected j itemModel =
                    if widget.isSelected itemModel && j /= i then
                        widget.update widget.unselectMsg itemModel (Index j :: p)
                    else
                        wrapWithNoCmd itemModel

                ( contents, unselectCmds ) =
                    List.unzip <| List.indexedMap unselectPreviouslySelected contentsWithWidgetUpdated

                finalCmd =
                    Cmd.batch <| updateCmd :: List.indexedMap (\i -> Cmd.map (ItemMsg i)) unselectCmds
            in
                ( { model | contents = contents }, finalCmd )


modelWithRemovedItem : Index -> Model newItemModel itemModel -> Model newItemModel itemModel
modelWithRemovedItem i model =
    { model | contents = List.take i model.contents ++ List.drop (i + 1) model.contents }


updateWidget : ItemWidget itemModel itemMsg -> Index -> itemMsg -> Widget.Path -> Index -> itemModel -> ( itemModel, Cmd itemMsg )
updateWidget widget refIndex msg p candidateIndex model =
    if candidateIndex == refIndex then
        widget.update msg model (Index candidateIndex :: p)
    else
        wrapWithNoCmd model



-- SUBSCRIPTIONS


subscriptions :
    ListBinding (Msg newItemMsg itemMsg itemModel) err
    -> ItemWidget itemModel itemMsg
    -> Model newItemModel itemModel
    -> Widget.Path
    -> Sub (Msg newItemMsg itemMsg itemModel)
subscriptions binding widget model p =
    let
        bindingToMsg msgBuilder =
            Sub.map (Result.withDefault NoOp << Result.map msgBuilder)

        itemSub i itemModel =
            Sub.map (ItemMsg i) <| widget.subscriptions itemModel (Index i :: p)
    in
        Sub.batch
            ([ bindingToMsg ModelAddedItem (binding.itemAdded p)
             , bindingToMsg ModelRemovedItem (binding.itemRemoved p)
             ]
                ++ List.indexedMap itemSub model.contents
            )



-- VIEW


view :
    NewItemWidget newItemModel newItemMsg
    -> ItemWidget itemModel itemMsg
    -> Model newItemModel itemModel
    -> Html (Msg newItemMsg itemMsg itemModel)
view newItemWidget widget model =
    ul [] <|
        li [] [ Html.map UINewItemMsg <| newItemWidget.view model.itemToAdd ]
            :: List.indexedMap (viewWidget widget) model.contents


viewWidget :
    ItemWidget itemModel itemMsg
    -> Index
    -> itemModel
    -> Html (Msg newItemMsg itemMsg itemModel)
viewWidget widget i m =
    li []
        [ span []
            [ Html.map (ItemMsg i) <| widget.view m
            , button [ onClick <| UIRemove i ] [ text "-" ]
            ]
        ]
