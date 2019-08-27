module Util.HashDict exposing (..)

import HashingContainers.HashDict as HashDict exposing (HashDict)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
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


monoid :
    Equality k
    -> Hashing k
    -> Semigroup v
    -> Monoid (HashDict k v)
monoid keyEquality keyHashing valueSemigroup =
    Monoid.semigroupAndIdentity
        { prepend = union valueSemigroup }
        (HashDict.empty keyEquality keyHashing)


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
