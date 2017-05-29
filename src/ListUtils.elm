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
substract : List t -> List t -> Maybe (List t)
substract l1 l2 =
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


remove : List t -> Int -> Maybe (List t)
remove l i =
    if (i < List.length l) then
        Just <| List.take i l ++ List.drop (i + 1) l
    else
        Nothing


get : List t -> Int -> Maybe t
get l i =
    if (i >= 0) then
        List.head (List.drop i l)
    else
        Nothing


firstMatch : (t -> Bool) -> List t -> Maybe Int
firstMatch p l =
    let
        firstMatchIndexed l i =
            case l of
                [] ->
                    Nothing

                x :: xs ->
                    if p x then
                        Just i
                    else
                        firstMatchIndexed xs (i + 1)
    in
        firstMatchIndexed l 0


unzip3 : List ( a, b, c ) -> ( List a, List b, List c )
unzip3 l =
    case l of
        ( x, y, z ) :: xs ->
            let
                ( l1, l2, l3 ) =
                    unzip3 xs
            in
                ( x :: l1, y :: l2, z :: l3 )

        [] ->
            ( [], [], [] )
