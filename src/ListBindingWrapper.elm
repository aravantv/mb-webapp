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


type alias Model wrappedModel =
    ( wrappedModel, IndexMapping )


getTranslatedIndex : IndexMapping -> Index -> BindingResult Index
getTranslatedIndex idxMap i =
    Binding.ofMaybe (IndexMapping.get idxMap i) "Index not found - please report"


type alias Item carriedValue =
    ( Index, carriedValue )


getTranslatedItem : IndexMapping -> Item carriedValue -> BindingResult (Item carriedValue)
getTranslatedItem idxMap ( i, v ) =
    getTranslatedIndex idxMap i |> Binding.map (\j -> ( j, v ))


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
    let
        msgOfAddedItem ( i, outVal ) =
            \idxMap ->
                case out2in outVal of
                    Binding.Ok inVal ->
                        let
                            newIdxMap =
                                IndexMapping.insert idxMap i
                        in
                            ( getTranslatedItem newIdxMap ( i, inVal ), newIdxMap )

                    Binding.Err err ->
                        ( (Binding.Err err), IndexMapping.insertButSkip idxMap i )

                    Binding.Irrelevant ->
                        ( Binding.Irrelevant, IndexMapping.insertButSkip idxMap i )

        msgOfModifiedItem ( i, outVal ) =
            \idxMap ->
                case out2in outVal of
                    Binding.Ok inVal ->
                        ( getTranslatedItem idxMap ( i, inVal ), idxMap )

                    Binding.Err err ->
                        ( (Binding.Err err), IndexMapping.insertButSkip idxMap i )

                    Binding.Irrelevant ->
                        ( Binding.Irrelevant, IndexMapping.insertButSkip idxMap i )

        msgOfRemovedIndex i =
            \idxMap ->
                ( getTranslatedIndex idxMap i, IndexMapping.remove idxMap i )

        msgOfFullList fullList =
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
    in
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
                                mapWrappedMsg symbolicSub.itemAdded <| mapBindingRes msgOfAddedItem resultItem
                        , itemModified =
                            \resultItem ->
                                mapWrappedMsg symbolicSub.itemModified <| mapBindingRes msgOfModifiedItem resultItem
                        , itemRemoved =
                            \resultIndex ->
                                mapWrappedMsg symbolicSub.itemRemoved <| mapBindingRes msgOfRemovedIndex resultIndex
                        , getFullList =
                            \resultFullList ->
                                mapWrappedMsg symbolicSub.getFullList <| mapBindingRes msgOfFullList resultFullList
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
