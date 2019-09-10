module Ucb.Util.Array exposing (..)

import Array exposing (Array)
import Misc exposing (impossible)


cons : a -> Array a -> Array a
cons =
    singleton >> Array.append


singleton : a -> Array a
singleton x =
    Array.push x Array.empty


unsafeLast : Array a -> a
unsafeLast xs =
    case Array.get (Array.length xs - 1) xs of
        Nothing ->
            impossible "unsafeLast: empty array"

        Just x ->
            x
