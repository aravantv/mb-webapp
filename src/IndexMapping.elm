module IndexMapping exposing (..)


type alias Index =
    Int


{-| Every pair (i,j) indicates that the object of index i in the original list,
  corresponds to index j in the target list. We always have i >= j.
-}
type alias IndexMapping =
    List Index


empty : IndexMapping
empty =
    []


{-| Returns the mapping after an object has been inserted at index [i] in the original list,
  and is added as well in the target list (in general will not be at index [i]).
-}
insert : IndexMapping -> Index -> IndexMapping
insert m i =
    case m of
        [] ->
            [ i ]

        j :: js ->
            if j < i then
                j :: insert js i
            else
                i :: List.map (\j -> j + 1) m


{-| Returns the mapping after an object has been inserted at index [i] in the original list,
  but is skipped in the target list.
-}
insertButSkip : IndexMapping -> Index -> IndexMapping
insertButSkip m i =
    case m of
        [] ->
            [ i ]

        j :: js ->
            if j < i then
                j :: insert js i
            else
                List.map (\j -> j + 1) m


{-| Returns the mapping after an object has been removed at index [i] from the original list.
-}
remove : IndexMapping -> Index -> IndexMapping
remove m i =
    case m of
        [] ->
            [ i ]

        j :: js ->
            if j < i then
                j :: remove js i
            else if j > i then
                List.map (\j -> j - 1) m
            else
                List.map (\j -> j - 1) js


{-| Returns the index in the target list of the object at index [i] in the original list if any.
-}
get : IndexMapping -> Index -> Maybe Index
get m i =
    let
        indexedGet indexedMap =
            case indexedMap of
                [] ->
                    Nothing

                ( j, k ) :: indexedMap2 ->
                    if j == i then
                        Just k
                    else
                        indexedGet indexedMap2
    in
        indexedGet <| List.indexedMap (\i x -> ( x, i )) m


retrieve : IndexMapping -> Index -> Maybe Index
retrieve m i =
    List.head (List.drop i m)
