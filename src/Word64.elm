module Word64 exposing (..)


type Word64
    = SmallWord64 Int
    | BigWord64 Int Int


{-| Take two 32-bit ints, representing the first and second half of a 64-bit
word, and smash 'em together.
-}
intsToWord64 :
    Int
    -> Int
    -> Word64
intsToWord64 x y =
    if x == 0 then
        SmallWord64 y

    else
        BigWord64 x y
