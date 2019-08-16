module Ucb.Util.Task exposing (fromResult)

import Task exposing (Task)


{-| Convert a Result into a Task.
-}
fromResult : Result a b -> Task a b
fromResult result =
    case result of
        Err err ->
            Task.fail err

        Ok ok ->
            Task.succeed ok
