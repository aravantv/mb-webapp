module EqualityConstraint exposing (..)

import ConstraintService exposing (Constraint, publishStatusChange, subscribeDataChange)
import ConstraintUtils exposing (ConstraintStatus, Fixes, fulfilled, onlyFix)
import Model exposing (Model)


type alias DataID =
    String


createConstraint : DataID -> DataID -> Constraint State Msg
createConstraint id1 id2 =
    { initState = initState
    , update = update
    , subscriptions = subscriptions id1 id2
    }



-- STATE


type Fix
    = Propagate1 Model
    | Propagate2 Model


type alias State =
    Maybe Fix


initState : State
initState =
    Nothing



-- MSG


type Msg t
    = Change1 ( t, t )
    | Change2 ( t, t )


update : Msg t -> State -> ( State, Cmd msg )
update msg _ =
    let
        newState =
            case msg of
                Change1 ( newVal1, existing2 ) ->
                    if (newVal1 == existing2) then
                        Nothing
                    else
                        Just (Propagate1 newVal1)

                Change2 ( existing1, newVal2 ) ->
                    if (newVal2 == existing1) then
                        Nothing
                    else
                        Just (Propagate2 newVal2)

        status =
            case newState of
                Nothing ->
                    fulfilled

                Just fix ->
                    onlyFix "Datas are different" <|
                        case fix of
                            Propagate1 m1 ->
                                { fixDescription = "Change second data"
                                , fix = \_ -> ( m1, m1 )
                                }

                            Propagate2 m2 ->
                                { fixDescription = "Change first data"
                                , fix = \_ -> ( m2, m2 )
                                }
    in
        ( newState, publishStatusChange status )



-- SUBSCRIPTIONS


subscriptions : DataID -> DataID -> State -> Sub Msg
subscriptions id1 id2 _ =
    Sub.batch [ subscribeDataChange id1 Propagate1, subscribeDataChange id2 Propagate2 ]
