module Unison.Util.Relation exposing (..)

import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet exposing (HashSet)
import Misc exposing (hashDictFromListWith, hashSetSingleton, hashSetUnion)
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
            hashSetUnion
            (List.map
                (\( x, y ) -> ( x, hashSetSingleton equalityB hashingB y ))
                elements
            )
    , range =
        hashDictFromListWith
            equalityB
            hashingB
            hashSetUnion
            (List.map
                (\( x, y ) -> ( y, hashSetSingleton equalityA hashingA x ))
                elements
            )
    }


relationRange :
    Relation a b
    -> List b
relationRange =
    .range >> HashDict.toList >> List.map Tuple.first
