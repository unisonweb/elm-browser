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


{-| This is "error"...
To compile with optimizations I guess we have to turn "Debug.todo" into an
infinite loop.
-}
impossible : String -> a
impossible s =
    -- impossible s
    Debug.todo s


listLast : List a -> Maybe a
listLast xs =
    case xs of
        [] ->
            Nothing

        [ x ] ->
            Just x

        y :: ys ->
            listLast ys


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
