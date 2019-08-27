module Util.HashSet exposing (..)

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Encode
import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import Typeclasses.Classes.Monoid as Monoid exposing (Monoid)
import Typeclasses.Classes.Semigroup exposing (Semigroup)


map :
    Equality b
    -> Hashing b
    -> (a -> b)
    -> HashSet a
    -> HashSet b
map equality hashing f =
    HashSet.foldl
        (\x -> HashSet.insert (f x))
        (HashSet.empty equality hashing)


singleton :
    Equality a
    -> Hashing a
    -> a
    -> HashSet a
singleton equality hashing x =
    HashSet.insert x (HashSet.empty equality hashing)


size :
    HashSet a
    -> Int
size =
    HashSet.toList >> List.length


monoid :
    Equality a
    -> Hashing a
    -> Monoid (HashSet a)
monoid valueEquality valueHashing =
    Monoid.semigroupAndIdentity
        semigroup
        (HashSet.empty valueEquality valueHashing)


semigroup : Semigroup (HashSet a)
semigroup =
    { prepend = union }


union :
    HashSet a
    -> HashSet a
    -> HashSet a
union =
    HashSet.foldl HashSet.insert


{-| TODO check to see if it is left- or right- biased, compare to haskell one
-}
unions :
    Equality a
    -> Hashing a
    -> List (HashSet a)
    -> HashSet a
unions equality hashing =
    List.foldl union (HashSet.empty equality hashing)
