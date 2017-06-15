module ListBindingWrapper exposing (..)

import Binding exposing (BindingResult, alwaysOk, filterIrrelevant, trivialErr)
import CollectionBinding exposing (BoundListWidget, mapCollectionPath, mapSymbolicCmd)
import Data exposing (AttributeValue(..), Data)
import Html exposing (sub)
import IndexMapping exposing (IndexMapping, Index)
import Widget exposing (TopWidget, Widget, cmdOf, cmdOfMsg, mapParamsSub, mapParamsUp, modelOf)


type alias Msg wrappedMsg =
    IndexMapping -> ( wrappedMsg, IndexMapping )


trivialMsg : wrappedMsg -> Msg wrappedMsg
trivialMsg m =
    \idxMap -> ( m, idxMap )


mapWrappedMsg : (wrappedMsg1 -> wrappedMsg2) -> Msg wrappedMsg1 -> Msg wrappedMsg2
mapWrappedMsg f msg idxMap =
    let
        ( innerMsg, newIdxMap ) =
            msg idxMap
    in
        ( f innerMsg, idxMap )


{-| Message in case the item at index [i] of the original list got removed.
  We adapt the mapping and inform the wrapped widget, only if the item was present in the converted list.
-}
msgOfRemovedIndex : Index -> Msg (BindingResult Index)
msgOfRemovedIndex i =
    \idxMap ->
        let
            wrappedMsg =
                case IndexMapping.get idxMap i of
                    Just j ->
                        Binding.Ok j

                    Nothing ->
                        Binding.Irrelevant
        in
            ( wrappedMsg, IndexMapping.remove idxMap i )


type alias Item carriedValue =
    ( Index, carriedValue )


{-| Message in case the item at index [i] of value [outVal] get added to the original list.
  We adapt the mapping and transmit the item to the wrapped widget only if conversion works out.
-}
msgOfMappedAddedItem : (outValue -> BindingResult inValue) -> Item outValue -> Msg (BindingResult (Item inValue))
msgOfMappedAddedItem out2in ( i, outVal ) =
    \idxMap ->
        case out2in outVal of
            Binding.Ok inVal ->
                let
                    ( newIdxMap, newIdx ) =
                        IndexMapping.insertAndGet idxMap i
                in
                    ( Binding.Ok ( newIdx, inVal ), newIdxMap )

            Binding.Err err ->
                ( (Binding.Err err), IndexMapping.insertButSkip idxMap i )

            Binding.Irrelevant ->
                ( Binding.Irrelevant, IndexMapping.insertButSkip idxMap i )


{-| Message in case the item at index [i] of the original list was modified into value [outVal].
  We need to consider various cases depending if the item was before already present in the converted list or not.
  If it was not, but the new value makes it present, then we do the same as for item addition.
  Conversely if it was present, but the new value makes it absent, then we do the same as for item removal.
  In other cases, we just change the values.
-}
msgOfMappedModifiedItem : (outValue -> BindingResult inValue) -> Item outValue -> Msg (BindingResult (Item inValue))
msgOfMappedModifiedItem out2in ( i, outVal ) =
    \idxMap ->
        case ( out2in outVal, IndexMapping.get idxMap i ) of
            ( Binding.Ok inVal, Just j ) ->
                ( Binding.Ok ( j, inVal ), idxMap )

            ( Binding.Ok inVal, Nothing ) ->
                let
                    ( newIdxMap, newIdx ) =
                        IndexMapping.insertAndGet idxMap i
                in
                    ( Binding.Ok ( newIdx, inVal ), newIdxMap )

            ( Binding.Err err, Nothing ) ->
                ( Binding.Irrelevant, idxMap )

            ( Binding.Err err, Just _ ) ->
                ( Binding.Err err, IndexMapping.remove idxMap i )

            ( Binding.Irrelevant, _ ) ->
                ( Binding.Irrelevant, idxMap )


{-| Message in case of receiving a full list.
  We just go through all elements and add them to the list which we send to the wrapped widget,
  only if the conversion works out.
-}
msgOfMappedFullList : (outValue -> BindingResult inValue) -> List outValue -> Msg (BindingResult (List inValue))
msgOfMappedFullList out2in fullList =
    \_ ->
        let
            itemOut2In outValue ( acc, idxMap, i ) =
                case out2in outValue of
                    Binding.Ok inValue ->
                        ( inValue :: acc, IndexMapping.insert idxMap i, i + 1 )

                    _ ->
                        ( acc, IndexMapping.insertButSkip idxMap i, i + 1 )

            ( newFullList, idxMap, _ ) =
                List.foldr itemOut2In ( [], IndexMapping.empty, 0 ) fullList
        in
            ( Binding.Ok newFullList, idxMap )


type alias Model wrappedModel =
    ( wrappedModel, IndexMapping )


type alias WrappedBoundListWidget wrappedModel wrappedMsg carriedValue =
    BoundListWidget (Model wrappedModel) (Msg wrappedMsg) carriedValue


mapBindingRes : (a -> Msg (BindingResult resType)) -> BindingResult a -> Msg (BindingResult resType)
mapBindingRes f res =
    case res of
        Binding.Ok v ->
            f v

        Binding.Err err ->
            trivialMsg (Binding.Err err)

        Binding.Irrelevant ->
            trivialMsg Binding.Irrelevant


makeListBindingWrapper :
    (inCarriedValue -> BindingResult outCarriedValue)
    -> (outCarriedValue -> BindingResult inCarriedValue)
    -> BoundListWidget innerModel innerMsg inCarriedValue
    -> WrappedBoundListWidget innerModel innerMsg outCarriedValue
makeListBindingWrapper in2out out2in w =
    { init = ( ( modelOf w.init, IndexMapping.empty ), Cmd.map trivialMsg (cmdOf w.init) )
    , update =
        \msg ( model, idxMap ) ->
            let
                ( subMsg, newIdxMap ) =
                    msg idxMap

                ( newModel, cmd, symbolicCmd ) =
                    w.update subMsg model

                newSymbolicCmd =
                    mapCollectionPath (\i -> IndexMapping.retrieve newIdxMap i) symbolicCmd
            in
                ( ( newModel, newIdxMap ), Cmd.map trivialMsg cmd, mapSymbolicCmd in2out newSymbolicCmd )
    , subscriptions =
        \( model, _ ) ->
            let
                ( sub, symbolicSub ) =
                    w.subscriptions model

                newSymbolicSub =
                    { itemAdded =
                        \resultItem ->
                            mapWrappedMsg symbolicSub.itemAdded <| mapBindingRes (msgOfMappedAddedItem out2in) resultItem
                    , itemModified =
                        \resultItem ->
                            mapWrappedMsg symbolicSub.itemModified <| mapBindingRes (msgOfMappedModifiedItem out2in) resultItem
                    , itemRemoved =
                        \resultIndex ->
                            mapWrappedMsg symbolicSub.itemRemoved <| mapBindingRes msgOfRemovedIndex resultIndex
                    , getFullList =
                        \resultFullList ->
                            mapWrappedMsg symbolicSub.getFullList <| mapBindingRes (msgOfMappedFullList out2in) resultFullList
                    }
            in
                ( Sub.map trivialMsg sub, newSymbolicSub )
    , view = \( model, _ ) -> Html.map trivialMsg (w.view model)
    }


stringOfIntBindingWrapper :
    BoundListWidget innerModel innerMsg Int
    -> WrappedBoundListWidget innerModel innerMsg String
stringOfIntBindingWrapper =
    makeListBindingWrapper (Binding.alwaysOk toString) (Binding.ofResult << String.toInt)


intOfStringBindingWrapper :
    BoundListWidget innerModel innerMsg String
    -> WrappedBoundListWidget innerModel innerMsg Int
intOfStringBindingWrapper =
    makeListBindingWrapper (Binding.ofResult << String.toInt) (Binding.alwaysOk toString)


stringOfData : Data.Data -> BindingResult String
stringOfData d =
    case d of
        Data.String s ->
            Binding.Ok s

        _ ->
            trivialErr "Not a string"


dataOfStringBindingWrapper :
    BoundListWidget innerModel innerMsg String
    -> WrappedBoundListWidget innerModel innerMsg Data.Data
dataOfStringBindingWrapper =
    makeListBindingWrapper (Binding.alwaysOk Data.String) stringOfData


stringOfDataBindingWrapper :
    BoundListWidget innerModel innerMsg Data.Data
    -> WrappedBoundListWidget innerModel innerMsg String
stringOfDataBindingWrapper =
    makeListBindingWrapper stringOfData (Binding.alwaysOk Data.String)


intOfData : Data.Data -> BindingResult Int
intOfData d =
    case d of
        Data.Int n ->
            Binding.Ok n

        _ ->
            trivialErr "Not an integer"


dataOfIntBindingWrapper :
    BoundListWidget innerModel innerMsg Int
    -> WrappedBoundListWidget innerModel innerMsg Data.Data
dataOfIntBindingWrapper =
    makeListBindingWrapper (Binding.alwaysOk Data.Int) intOfData


intOfDataBindingWrapper :
    BoundListWidget innerModel innerMsg Data.Data
    -> WrappedBoundListWidget innerModel innerMsg Int
intOfDataBindingWrapper =
    makeListBindingWrapper intOfData (Binding.alwaysOk Data.Int)


makeListBindingFilter :
    (carriedValue -> Bool)
    -> BoundListWidget innerModel innerMsg carriedValue
    -> WrappedBoundListWidget innerModel innerMsg carriedValue
makeListBindingFilter p =
    let
        filter v =
            if p v then
                Binding.Ok v
            else
                trivialErr "makeListBindingFilter: filter not satisfied"
    in
        makeListBindingWrapper Binding.Ok filter
