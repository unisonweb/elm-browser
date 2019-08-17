module Unison.Util.Relation exposing (..)

import Dict.Any exposing (AnyDict)
import HashingContainers
import Set.Any exposing (AnySet)


{-| Haskell type: Unison.Util.Relation.Relation
-}
type alias Relation a b =
    { domain : HashingContainers.HashDict a (HashingContainers.HashSet b)
    , range : HashingContainers.HashDict b (HashingContainers.HashSet a)
    }
