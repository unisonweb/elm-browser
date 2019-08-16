module Unison.Util.Star3 exposing (..)

import Set.Any exposing (AnySet)
import Unison.Util.Relation exposing (Relation)


{-| Haskell type: Unison.Util.Star.Star3

Underscore in the name so you can locally alias a way a few of the type
variables and call it Star3.

-}
type alias Star3_ compareFact compareD1 compareD2 compareD3 fact d1 d2 d3 =
    { fact : AnySet compareFact fact
    , d1 : Relation compareFact compareD1 fact d1
    , d2 : Relation compareFact compareD2 fact d2
    , d3 : Relation compareFact compareD3 fact d3
    }
