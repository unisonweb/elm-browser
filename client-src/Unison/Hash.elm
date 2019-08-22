module Unison.Hash exposing
    ( Hash
    , Hash32
    , encodeHash
    , hash32Equality
    , hash32Hashing
    , hashHash32
    )

import Bitwise exposing (and, or, shiftLeftBy, shiftRightZfBy)
import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Misc exposing (impossible)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)


{-| Haskell type: Unison.Hash.Hash
-}
type alias Hash =
    Bytes


{-| A base32hex-encoded hash.
-}
type alias Hash32 =
    String


hash32Equality : Equality Hash32
hash32Equality =
    Equality.string


hash32Hashing : Hashing Hash32
hash32Hashing =
    Hashing.hash hashHash32


{-| base32hex encode, with padding stripped.
-}
encodeHash :
    Hash
    -> Hash32
encodeHash bytes =
    case Decode.decode (decoder (Bytes.width bytes)) bytes of
        Nothing ->
            impossible "encodeHash: decoder failed?"

        Just string ->
            string


decoder :
    Int
    -> Decoder String
decoder width =
    Decode.loop ( width, "" ) step


step :
    ( Int, String )
    -> Decoder (Step ( Int, String ) String)
step ( remaining, acc ) =
    if remaining >= 5 then
        let
            step5 : Int -> Int -> Step ( Int, String ) String
            step5 a b =
                let
                    decoded : String
                    decoded =
                        String.fromList
                            [ chr (shiftRightZfBy 27 a)
                            , chr (and (shiftRightZfBy 22 a) 31)
                            , chr (and (shiftRightZfBy 17 a) 31)
                            , chr (and (shiftRightZfBy 12 a) 31)
                            , chr (and (shiftRightZfBy 7 a) 31)
                            , chr (and (shiftRightZfBy 2 a) 31)
                            , chr (or (shiftLeftBy 3 (and a 3)) (shiftRightZfBy 5 b))
                            , chr (and b 31)
                            ]
                in
                Loop
                    ( remaining - 5
                    , acc ++ decoded
                    )
        in
        Decode.map2
            step5
            (Decode.unsignedInt32 Bytes.BE)
            Decode.unsignedInt8

    else if remaining == 4 then
        let
            step4 : Int -> Step ( Int, String ) String
            step4 a =
                let
                    decoded : String
                    decoded =
                        String.fromList
                            [ chr (shiftRightZfBy 27 a)
                            , chr (and (shiftRightZfBy 22 a) 31)
                            , chr (and (shiftRightZfBy 17 a) 31)
                            , chr (and (shiftRightZfBy 12 a) 31)
                            , chr (and (shiftRightZfBy 7 a) 31)
                            , chr (and (shiftRightZfBy 2 a) 31)
                            , chr (shiftLeftBy 3 (and a 3))
                            ]
                in
                Done (acc ++ decoded)
        in
        Decode.map
            step4
            (Decode.unsignedInt32 Bytes.BE)

    else if remaining == 3 then
        let
            step3 : Int -> Int -> Step ( Int, String ) String
            step3 a b =
                let
                    decoded : String
                    decoded =
                        String.fromList
                            [ chr (shiftRightZfBy 11 a)
                            , chr (and (shiftRightZfBy 6 a) 31)
                            , chr (and (shiftRightZfBy 1 a) 31)
                            , chr (or (shiftLeftBy 4 (and a 1)) (shiftRightZfBy 4 b))
                            , chr (and (shiftRightZfBy 3 b) 30)
                            ]
                in
                Done (acc ++ decoded)
        in
        Decode.map2
            step3
            (Decode.unsignedInt16 Bytes.BE)
            Decode.unsignedInt8

    else if remaining == 2 then
        let
            step2 : Int -> Step ( Int, String ) String
            step2 a =
                let
                    decoded : String
                    decoded =
                        String.fromList
                            [ chr (shiftRightZfBy 11 a)
                            , chr (and (shiftRightZfBy 6 a) 31)
                            , chr (and (shiftRightZfBy 1 a) 31)
                            , chr (shiftLeftBy 4 (and a 1))
                            ]
                in
                Done (acc ++ decoded)
        in
        Decode.map
            step2
            (Decode.unsignedInt16 Bytes.BE)

    else if remaining == 1 then
        let
            step1 : Int -> Step ( Int, String ) String
            step1 a =
                let
                    decoded : String
                    decoded =
                        String.fromList
                            [ chr (shiftRightZfBy 3 a)
                            , chr (shiftLeftBy 2 (and a 7))
                            ]
                in
                Done (acc ++ decoded)
        in
        Decode.map
            step1
            Decode.unsignedInt8

    else
        Decode.succeed (Done acc)


chr : Int -> Char
chr n =
    if n <= 9 then
        -- digits
        Char.fromCode (n + 48)

    else
        -- and lowercase letters
        Char.fromCode (n + 87)


{-| Hash-ception... hashing a Hash32 to an Int for use in hashable containers.
This function just hard-codes a decent(-seeming) number of characters to use in
the hash.
-}
hashHash32 : Hash32 -> Int
hashHash32 =
    (Hashing.string 6).hash
