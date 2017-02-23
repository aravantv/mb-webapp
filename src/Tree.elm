module Tree exposing (..)

import ListUtils
import Widget exposing (TreePath)


type Tree a
    = Node (List ( a, Tree a ))


empty : Tree a
empty =
    Node []


applyAtPath :
    Tree a
    -> (List ( a, Tree a ) -> Int -> Maybe (List ( b, Tree b )))
    -> TreePath
    -> Maybe (Tree b)
applyAtPath (Node l) f path =
    case path of
        [] ->
            Nothing

        [ i ] ->
            Maybe.map Node <| f l i

        i :: j :: is ->
            case ListUtils.get l i of
                Just ( _, subTree ) ->
                    applyAtPath subTree f (j :: is)

                Nothing ->
                    Nothing


insert : Tree a -> a -> TreePath -> Maybe (Tree a)
insert t x path =
    applyAtPath t (\l i -> ListUtils.insert l ( x, empty ) i) path


remove : Tree a -> TreePath -> Maybe (Tree a)
remove t path =
    applyAtPath t ListUtils.remove path


indexedMap : Tree a -> (TreePath -> a -> b) -> Tree b
indexedMap t f =
    let
        rec (Node l) currentPath =
            let
                listF i ( a, subTree ) =
                    let
                        newPath =
                            currentPath ++ [ i ]
                    in
                        ( f newPath a, rec subTree newPath )
            in
                Node <| List.indexedMap listF l
    in
        rec t []
