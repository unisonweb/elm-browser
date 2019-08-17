module Int64 exposing (..)


type Int64
    = SmallInt64 Int
    | BigInt64 Int Int

{-| Take two 32-bit ints, representing the first and second half of a 64-bit
word, and smash 'em together.
-}
intsToInt64 :
    Int
    -> Int
    -> Int64
intsToInt64 x y =
    if x == 0 then
        SmallInt64 y

    else
        BigInt64 x y
