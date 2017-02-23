module ListUtils exposing (..)


insert : List t -> t -> Int -> Maybe (List t)
insert l x i =
    if i == 0 then
        Just (x :: l)
    else
        case l of
            [] ->
                Nothing

            y :: ys ->
                Maybe.map (\l -> y :: l) <| insert ys x (i - 1)


{-| substract [x,y,z,t] [z,t] === [x,y]
-}
listSubstract : List t -> List t -> Maybe (List t)
listSubstract l1 l2 =
    let
        diff_length =
            List.length l1 - List.length l2
    in
        if diff_length < 0 then
            Nothing
        else if List.drop diff_length l1 == l2 then
            Just (List.take diff_length l1)
        else
            Nothing


listRemove : List t -> Int -> List t
listRemove l i =
    List.take i l ++ List.drop (i + 1) l


get : List t -> Int -> Maybe t
get l i =
    List.head (List.drop i l)