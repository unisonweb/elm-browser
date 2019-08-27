module Unison.Util.Relation exposing (..)

import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Typeclasses.Classes.Equality exposing (Equality)
import Typeclasses.Classes.Hashing exposing (Hashing)
import Util.HashDict as HashDict
import Util.HashSet as HashSet


{-| Haskell type: Unison.Util.Relation.Relation
-}
type alias Relation a b =
    { domain : HashDict a (HashSet b)
    , range : HashDict b (HashSet a)
    }


emptyRelation :
    Equality a
    -> Hashing a
    -> Equality b
    -> Hashing b
    -> Relation a b
emptyRelation ea ha eb hb =
    { domain = HashDict.empty ea ha
    , range = HashDict.empty eb hb
    }


relationFromList :
    Equality a
    -> Hashing a
    -> Equality b
    -> Hashing b
    -> List ( a, b )
    -> Relation a b
relationFromList ea ha eb hb elements =
    { domain =
        HashDict.fromListWith
            ea
            ha
            HashSet.union
            (List.map
                (\( x, y ) -> ( x, HashSet.singleton eb hb y ))
                elements
            )
    , range =
        HashDict.fromListWith
            eb
            hb
            HashSet.union
            (List.map
                (\( x, y ) -> ( y, HashSet.singleton ea ha x ))
                elements
            )
    }


relationToList :
    Relation a b
    -> List ( a, b )
relationToList =
    .domain
        >> HashDict.toList
        >> List.concatMap
            (\( a, bs ) ->
                List.map (\b -> ( a, b )) (HashSet.toList bs)
            )


relationRange :
    Relation a b
    -> List b
relationRange =
    .range >> HashDict.toList >> List.map Tuple.first


relationMapRange :
    Equality a
    -> Hashing a
    -> Equality c
    -> Hashing c
    -> (b -> c)
    -> Relation a b
    -> Relation a c
relationMapRange ea ha ec hc f =
    relationToList
        >> List.map (Tuple.mapSecond f)
        >> relationFromList ea ha ec hc


relationUnion :
    Relation a b
    -> Relation a b
    -> Relation a b
relationUnion r1 r2 =
    { domain = HashDict.union HashSet.semigroup r1.domain r2.domain
    , range = HashDict.union HashSet.semigroup r1.range r2.range
    }
