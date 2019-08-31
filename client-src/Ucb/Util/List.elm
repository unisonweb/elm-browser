module Ucb.Util.List exposing (..)

import Task exposing (Task)


traverseTask :
    (a -> Task x b)
    -> List a
    -> Task x (List b)
traverseTask f xs =
    Debug.todo ""
