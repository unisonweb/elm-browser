module Unison.Util.Relation exposing (..)

import HashingContainers.HashDict exposing (HashDict)
import HashingContainers.HashSet exposing (HashSet)
import Misc exposing (hashDictFromListWith)
import Typeclasses.Classes.Equality exposing (Equality)
import Typeclasses.Classes.Hashing exposing (Hashing)


{-| Haskell type: Unison.Util.Relation.Relation
-}
type alias Relation a b =
    { domain : HashDict a (HashSet b)
    , range : HashDict b (HashSet a)
    }


relationFromList :
    Equality a
    -> Hashing a
    -> Equality b
    -> Hashing b
    -> List ( a, b )
    -> Relation a b
relationFromList equalityA hashingA equalityB hashingB elements =
    { domain =
        hashDictFromListWith
            equalityA
            hashingA
            (\x y -> Debug.todo "x")
            (Debug.todo "y")
    , range =
        hashDictFromListWith
            equalityB
            hashingB
            (Debug.todo "z")
            (Debug.todo "a")
    }
