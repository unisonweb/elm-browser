module Ucb.Util.List exposing (..)

import Task as Task exposing (Task)


cons :
    a
    -> List a
    -> List a
cons x xs =
    x :: xs


traverseTask :
    (a -> Task x b)
    -> List a
    -> Task x (List b)
traverseTask f xs =
    case xs of
        [] ->
            Task.succeed []

        y :: ys ->
            Task.map2
                (::)
                (f y)
                (traverseTask f ys)
