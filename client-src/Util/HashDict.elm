module Util.HashDict exposing (..)

import HashingContainers.HashDict as HashDict exposing (HashDict)
import Typeclasses.Classes.Equality exposing (Equality)
import Typeclasses.Classes.Hashing exposing (Hashing)
import Typeclasses.Classes.Monoid as Monoid exposing (Monoid)
import Typeclasses.Classes.Semigroup exposing (Semigroup)


fromListWith :
    Equality k
    -> Hashing k
    -> (v -> v -> v)
    -> List ( k, v )
    -> HashDict k v
fromListWith equality hashing combine =
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


map :
    Equality k
    -> Hashing k
    -> (a -> b)
    -> HashDict k a
    -> HashDict k b
map equality hashing f =
    HashDict.foldl
        (\( k, v ) -> HashDict.insert k (f v))
        (HashDict.empty equality hashing)


mapKeys :
    Equality k2
    -> Hashing k2
    -> (k1 -> k2)
    -> HashDict k1 a
    -> HashDict k2 a
mapKeys equality hashing f =
    HashDict.foldl
        (\( k, v ) -> HashDict.insert (f k) v)
        (HashDict.empty equality hashing)


monoid :
    Equality k
    -> Hashing k
    -> Semigroup v
    -> Monoid (HashDict k v)
monoid keyEquality keyHashing valueSemigroup =
    Monoid.semigroupAndIdentity
        { prepend = union valueSemigroup }
        (HashDict.empty keyEquality keyHashing)


{-| The smaller dict should come first.
-}
union :
    Semigroup v
    -> HashDict k v
    -> HashDict k v
    -> HashDict k v
union { prepend } xs ys =
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
