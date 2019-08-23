module Word64 exposing (..)

import Bitwise
import Misc exposing (tumble)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)


type Word64
    = SmallWord64 Int
    | BigWord64 Int Int


word64Hashing : Hashing Word64
word64Hashing =
    Hashing.hash
        (\w ->
            case w of
                SmallWord64 n ->
                    n

                BigWord64 n m ->
                    tumble n m
        )


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


{-| Unsafely convert a Word64 to a 53-bit JavaScript number.
-}
unsafeWord64ToWord53 : Word64 -> Int
unsafeWord64ToWord53 word =
    case word of
        SmallWord64 n ->
            n

        BigWord64 n m ->
            Bitwise.or (Bitwise.shiftLeftBy 32 n) m
