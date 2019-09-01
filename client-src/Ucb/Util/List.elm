module Ucb.Util.List exposing (..)

import Task as Task exposing (Task)


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
