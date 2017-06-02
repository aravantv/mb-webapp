module IndexMapping exposing (..)


type alias Index =
    Int


{-| Every pair (i,j) indicates that the object of index i in the original list,
  corresponds to index j in the target list. We always have i >= j.
-}
type alias IndexMapping =
    List ( Index, Index )


empty : IndexMapping
empty =
    []


{-| Returns the mapping after an object has been inserted at index [i] in the original list,
  and is added as well in the target list (in general will not be at index [i]).
-}
insert : IndexMapping -> Index -> IndexMapping
insert m i =
    let
        insertAux m i j =
            case m of
                [] ->
                    [ ( i, j ) ]

                ( j, k ) :: m2 ->
                    if j < i then
                        ( j, k ) :: insertAux m2 i (k + 1)
                    else
                        ( i, k ) :: List.map (\( j, k ) -> ( j + 1, k + 1 )) m2
    in
        insertAux m i 0


{-| Returns the mapping after an object has been inserted at index [i] in the original list,
  but is skipped in the target list.
-}
insertButSkip : IndexMapping -> Index -> IndexMapping
insertButSkip m i =
    List.map
        (\( j, k ) ->
            if j < i then
                ( j, k )
            else
                ( j + 1, k )
        )
        m


{-| Returns the mapping after an object has been removed at index [i] from the original list.
-}
remove : IndexMapping -> Index -> IndexMapping
remove m i =
    case m of
        [] ->
            []

        ( j, k ) :: m2 ->
            if j < i then
                ( j, k ) :: remove m2 i
            else if j == i then
                List.map (\( j, k ) -> ( j - 1, k - 1 )) m
            else
                m


{-| Returns the index in the target list of the object at index [i] in the original list.
-}
get : IndexMapping -> Index -> Maybe Index
get m i =
    case m of
        [] ->
            Nothing

        ( j, k ) :: m2 ->
            if j == i then
                Just k
            else
                get m2 i
