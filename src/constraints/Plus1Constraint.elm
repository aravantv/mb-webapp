module EqualityConstraint exposing (..)

import ConstraintUtils exposing (ConstraintStatus, Fixes, fulfilled, onlyFix)


-- MODEL


type Fix
    = Propagate1 Int
    | Propagate2 Int


type alias Model =
    Maybe Fix



-- MSG


type Msg
    = Change1 ( Int, Int )
    | Change2 ( Int, Int )


update : Msg -> Model -> Model
update msg _ =
    case msg of
        Change1 ( newVal1, existing2 ) ->
            if (newVal1 == existing2 + 1) then
                Nothing
            else
                Just (Propagate1 newVal1)

        Change2 ( existing1, newVal2 ) ->
            if (newVal2 + 1 == existing1) then
                Nothing
            else
                Just (Propagate2 newVal2)



-- VIEW


view : Model -> ConstraintStatus ( Int, Int )
view f =
    case f of
        Nothing ->
            fulfilled

        Just fix ->
            onlyFix "Datas are different" <|
                case fix of
                    Propagate1 m1 ->
                        { fixDescription = "Change second data"
                        , fix = \_ -> ( m1, m1 - 1 )
                        }

                    Propagate2 m2 ->
                        { fixDescription = "Change first data"
                        , fix = \_ -> ( m2 + 1, m2 )
                        }
