module Misc exposing (..)

import Bitwise
import Typeclasses.Classes.Hashing as Hashing


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
