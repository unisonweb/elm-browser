module Unison.Util.Relation exposing (..)

import Dict.Any exposing (AnyDict)
import Set.Any exposing (AnySet)


{-| Haskell type: Unison.Util.Relation.Relation
-}
type alias Relation compareA compareB a b =
    { domain : AnyDict compareA a (AnySet compareB b)
    , range : AnyDict compareB b (AnySet compareA a)
    }
