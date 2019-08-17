module Unison.Util.Relation exposing (..)

import HashingContainers.HashDict exposing (HashDict)
import HashingContainers.HashSet exposing (HashSet)


{-| Haskell type: Unison.Util.Relation.Relation
-}
type alias Relation a b =
    { domain : HashDict a (HashSet b)
    , range : HashDict b (HashSet a)
    }
