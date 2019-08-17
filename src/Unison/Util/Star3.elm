module Unison.Util.Star3 exposing (..)

import HashingContainers.HashSet exposing (HashSet)
import Unison.Util.Relation exposing (Relation)


{-| Haskell type: Unison.Util.Star.Star3

Underscore in the name so you can locally alias a way a few of the type
variables and call it Star3.

-}
type alias Star3_ fact d1 d2 d3 =
    { fact : HashSet fact
    , d1 : Relation fact d1
    , d2 : Relation fact d2
    , d3 : Relation fact d3
    }
