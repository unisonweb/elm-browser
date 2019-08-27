module Int64 exposing (..)

import Bitwise


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


{-| Unsafely convert an Int64 to a 53-bit JavaScript number.
-}
unsafeInt64ToInt53 : Int64 -> Int
unsafeInt64ToInt53 word =
    case word of
        SmallInt64 n ->
            n

        BigInt64 n m ->
            Debug.todo "unsafeInt64ToInt53: BigInt64"
