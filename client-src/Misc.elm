module Misc exposing (..)

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Encode
import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import Typeclasses.Classes.Monoid as Monoid exposing (Monoid)
import Typeclasses.Classes.Semigroup exposing (Semigroup)


emptyBytes : Bytes
emptyBytes =
    Bytes.Encode.encode (Bytes.Encode.sequence [])


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


hashDictMonoid :
    Equality k
    -> Hashing k
    -> Semigroup v
    -> Monoid (HashDict k v)
hashDictMonoid keyEquality keyHashing valueSemigroup =
    Monoid.semigroupAndIdentity
        { prepend = hashDictUnion valueSemigroup }
        (HashDict.empty keyEquality keyHashing)


hashDictUnion :
    Semigroup v
    -> HashDict k v
    -> HashDict k v
    -> HashDict k v
hashDictUnion { prepend } xs ys =
    HashDict.foldl
        (\( key, newValue ) ->
            HashDict.update
                key
                (\maybeOldValue ->
                    case maybeOldValue of
                        Nothing ->
                            Just newValue

                        Just oldValue ->
                            Just (prepend newValue oldValue)
                )
        )
        ys
        xs


hashSetSingleton :
    Equality a
    -> Hashing a
    -> a
    -> HashSet a
hashSetSingleton equality hashing x =
    HashSet.insert x (HashSet.empty equality hashing)


hashSetSize :
    HashSet a
    -> Int
hashSetSize =
    HashSet.toList >> List.length


hashSetMonoid :
    Equality a
    -> Hashing a
    -> Monoid (HashSet a)
hashSetMonoid valueEquality valueHashing =
    Monoid.semigroupAndIdentity
        hashSetSemigroup
        (HashSet.empty valueEquality valueHashing)


hashSetSemigroup : Semigroup (HashSet a)
hashSetSemigroup =
    { prepend = hashSetUnion }


{-| TODO upstream this
-}
hashSetUnion :
    HashSet a
    -> HashSet a
    -> HashSet a
hashSetUnion =
    HashSet.foldl HashSet.insert


{-| TODO upstream this
TODO check to see if it is left- or right- biased, compare to haskell one
-}
hashSetUnions :
    Equality a
    -> Hashing a
    -> List (HashSet a)
    -> HashSet a
hashSetUnions equality hashing =
    List.foldl hashSetUnion (HashSet.empty equality hashing)


{-| This is "error"...
-}
impossible : String -> a
impossible s =
    impossible s


maybe : b -> (a -> b) -> Maybe a -> b
maybe n j mx =
    case mx of
        Nothing ->
            n

        Just x ->
            j x


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


unsafeIndex : Int -> List a -> a
unsafeIndex n xs =
    case xs of
        [] ->
            impossible "unsafeIndex: empty list"

        y :: ys ->
            if n == 0 then
                y

            else
                unsafeIndex (n - 1) ys
