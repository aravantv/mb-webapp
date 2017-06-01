module IndexMapping exposing (..)


type alias Index =
    Int


{-| Every index in the list denotes an index which is skipped
-}
type alias IndexMapping =
    List ( Index, Index )


empty : IndexMapping
empty =
    []


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


insert : IndexMapping -> Index -> IndexMapping
insert m i =
    case m of
        [] ->
            [ ( i, 0 ) ]

        ( j, k ) :: m2 ->
            if j < i then
                ( j, k ) :: insert m2 i
            else
                ( i, k ) :: List.map (\( j, k ) -> ( j + 1, k + 1 )) m


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
