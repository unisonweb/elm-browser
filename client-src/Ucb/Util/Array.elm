module Ucb.Util.Array exposing (..)

import Array exposing (Array)


singleton : a -> Array a
singleton x =
    Array.push x Array.empty
