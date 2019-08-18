module Misc exposing (..)

import Bitwise
import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)


{-| TODO upstream this
-}
hashDictFromListWith :
    Equality k
    -> Hashing k
    -> (v -> v -> v)
    -> List ( k, v )
    -> HashDict k v
hashDictFromListWith equality hashing combine =
    List.foldl
        (\( k, v ) ->
            HashDict.update
                k
                (\mw ->
                    case mw of
                        Nothing ->
                            Just v

                        Just w ->
                            Just (combine w v)
                )
        )
        (HashDict.empty equality hashing)


hashSetSingleton :
    Equality a
    -> Hashing a
    -> a
    -> HashSet a
hashSetSingleton equality hashing x =
    HashSet.insert x (HashSet.empty equality hashing)


{-| TODO upstream this
-}
hashSetUnion :
    Equality a
    -> Hashing a
    -> HashSet a
    -> HashSet a
    -> HashSet a
hashSetUnion equality hashing =
    HashSet.foldl HashSet.insert


pairHashing :
    Hashing a
    -> Hashing b
    -> Hashing ( a, b )
pairHashing hashingA hashingB =
    Hashing.hash
        (\( a, b ) ->
            tumble
                (hashingA.hash a)
                (hashingB.hash b)
        )


{-| Taken from haskell 'hashable', but with args flipped so the salt comes
second. This lets you write tumble chains with postfix function application
like

@
initialSalt
|> tumble thingOne
|> tumble thingTwo
@

-}
tumble : Int -> Int -> Int
tumble thing salt =
    Bitwise.xor (salt * 16777619) thing
