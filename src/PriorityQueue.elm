module PriorityQueue exposing (create, dequeue, enqueue, front)

import List exposing (..)
import Util exposing (getMinToFront)


type alias Depth =
    Int


type Tree comparable
    = Node comparable Depth (List (Tree comparable))


treeUnion : Tree comparable -> Tree comparable -> Tree comparable
treeUnion t1 t2 =
    case t1 of
        Node v1 d1 f1 ->
            case t2 of
                Node v2 d2 f2 ->
                    if v1 < v2 then
                        Node v1 (d1 + 1) (t2 :: f1)

                    else
                        Node v2 (d2 + 1) (t1 :: f2)


type alias Forest comparable =
    List (Tree comparable)


type FibHeap comparable
    = Heap (Forest comparable)
    | Empty


create : comparable -> FibHeap comparable
create value =
    Heap [ Node value 1 [] ]


enqueue : comparable -> FibHeap comparable -> FibHeap comparable
enqueue value fh =
    case fh of
        Heap (head :: forest) ->
            case head of
                Node old_min _ _ ->
                    Heap
                        (if old_min < value then
                            head
                                :: Node value 1 []
                                :: forest

                         else
                            Node value 1 []
                                :: head
                                :: forest
                        )

        _ ->
            create value



-- union fh (create value)


organize_helper : Tree comparable -> Forest comparable -> Forest comparable
organize_helper t acc_f =
    case acc_f of
        [] ->
            [ t ]

        h :: rest ->
            case ( h, t ) of
                ( Node _ h_d _, Node _ t_d _ ) ->
                    if t_d == h_d then
                        organize_helper (treeUnion t h) rest

                    else
                        t :: acc_f


organize : Forest comparable -> Forest comparable
organize f =
    case f of
        [] ->
            []

        _ ->
            let
                sorted_forest =
                    List.sortBy (\(Node _ d _) -> d) f
            in
            sorted_forest
                |> List.foldr organize_helper []


dequeue : FibHeap comparable -> FibHeap comparable
dequeue fh =
    case fh of
        Empty ->
            fh

        Heap forest ->
            case forest of
                [] ->
                    Empty

                [ Node _ _ [] ] ->
                    Empty

                (Node _ _ cs) :: rest ->
                    cs
                        ++ rest
                        |> organize
                        |> getMinToFront (\(Node v _ _) -> v)
                        |> Heap


front : FibHeap comparable -> Maybe comparable
front fh =
    case fh of
        Empty ->
            Nothing

        Heap forest ->
            case forest of
                [] ->
                    Nothing

                [ Node _ _ [] ] ->
                    Nothing

                (Node v _ _) :: _ ->
                    Just v
