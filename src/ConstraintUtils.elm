module ConstraintUtils exposing (..)

import Data exposing (Data)


type alias PossibleFix =
    { fixDescription : String
    , fix : () -> Data
    }


type Fixes
    = OnlyPossibleFix PossibleFix
    | PossibleFixes (List PossibleFix)


type alias UnfulfillmentInfo =
    { unfulfillmentDescription : String
    , fixes : Fixes
    }


trivialUnfulfillmentInfo : String -> UnfulfillmentInfo
trivialUnfulfillmentInfo desc =
    { unfulfillmentDescription = desc
    , fixes = PossibleFixes []
    }


type ConstraintStatus
    = FulFilled
    | Unfulfilled UnfulfillmentInfo
    | Outdated
    | VerificationError UnfulfillmentInfo


type alias Constrained =
    Data


fulfilled : ConstraintStatus
fulfilled =
    FulFilled


onlyFix : String -> PossibleFix -> ConstraintStatus
onlyFix desc fix =
    Unfulfilled
        { unfulfillmentDescription = desc
        , fixes = OnlyPossibleFix fix
        }


multiFix : String -> List PossibleFix -> ConstraintStatus
multiFix desc fixes =
    Unfulfilled
        { unfulfillmentDescription = desc
        , fixes = PossibleFixes fixes
        }


unfulfilled : String -> ConstraintStatus
unfulfilled desc =
    multiFix desc []
