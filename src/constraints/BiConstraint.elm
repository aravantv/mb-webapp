module Constraint exposing (..)

import ConstraintUtils exposing (ConstraintStatus, Fixes)


-- MODEL


type Fix
    = Propagate1
    | Propagate2


type alias Model t1 t2 =
    ( t1, t2, Maybe Fix )



-- MSG


type Msg t1 t2
    = Change1 t1
    | Change2 t2


update : Msg t1 t2 -> Model t1 t2 -> Model t1 t2
update msg ( m1, m2, _ ) =
    case msg of
        Change1 newVal1 ->
            let
                s =
                    if (newVal1 == m2) then
                        Nothing
                    else
                        Just Propagate1
            in
                ( newVal1, m2, s )

        Change2 newVal2 ->
            let
                s =
                    if (newVal2 == m1) then
                        Nothing
                    else
                        Just Propagate1
            in
                ( m1, newVal2, s )



-- VIEW


view : Model t1 t2 -> ConstraintStatus ( t1, t2 )
view ( m1, m2, f ) =
    case f of
        Nothing ->
            ConstraintUtils.FulFilled

        Just fix ->
            ConstraintUtils.Unfulfilled
                { unfulfillmentDescription = "Datas are different"
                , fixes =
                    ConstraintUtils.OnlyPossibleFix <|
                        case fix of
                            Propagate1 ->
                                { fixDescription = "Propagate first data to second data"
                                , fix = \_ -> ( m1, m1 )
                                }

                            Propagate2 ->
                                { fixDescription = "Propagate second data to first data"
                                , fix = \_ -> ( m2, m2 )
                                }
                }



{--
plusOneConstraint : BiConstraint Int Int
plusOneConstraint =
    { constraintID = "plusOneConstraint"
    , verificationPreprocessor = identity
    , verify =
        \( x1, x2 ) ->
            if (x1 == x2 + 1) then
                FulFilled
            else
                Unfulfilled
                    [ { unfulfillmentDescription = "Datas are different"
                      , fixes = OnlyPossibleFix { fixDescription = "change first data", fix = \_ -> ( x2 + 1, x2 ) }
                      }
                    , { unfulfillmentDescription = "Datas are different"
                      , fixes = OnlyPossibleFix { fixDescription = "change second data", fix = \_ -> ( x1, x1 - 1 ) }
                      }
                    ]
    }-}
