module Ucb.Util.Array exposing (..)

import Array exposing (Array)


cons : a -> Array a -> Array a
cons =
    singleton >> Array.append


singleton : a -> Array a
singleton x =
    Array.push x Array.empty
