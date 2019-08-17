module Unison.Hash exposing
    ( Hash
    , encodeHash
    )

import Bitwise exposing (and, or, shiftLeftBy, shiftRightBy)
import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder, Step(..))


{-| Haskell type: Unison.Hash.Hash
-}
type alias Hash =
    Bytes


{-| base32hex encode, with padding stripped.
-}
encodeHash :
    Bytes
    -> String
encodeHash bytes =
    case Decode.decode (decoder (Bytes.width bytes)) bytes of
        Nothing ->
            impossible ()

        Just string ->
            string


impossible : () -> String
impossible _ =
    impossible ()


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
                            [ chr (shiftRightBy 27 a)
                            , chr (and (shiftRightBy 22 a) 31)
                            , chr (and (shiftRightBy 17 a) 31)
                            , chr (and (shiftRightBy 12 a) 31)
                            , chr (and (shiftRightBy 7 a) 31)
                            , chr (and (shiftRightBy 2 a) 31)
                            , chr (or (shiftLeftBy 3 (and a 3)) (shiftRightBy 5 b))
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
                            [ chr (shiftRightBy 27 a)
                            , chr (and (shiftRightBy 22 a) 31)
                            , chr (and (shiftRightBy 17 a) 31)
                            , chr (and (shiftRightBy 12 a) 31)
                            , chr (and (shiftRightBy 7 a) 31)
                            , chr (and (shiftRightBy 2 a) 31)
                            , chr (shiftLeftBy 2 (and a 3))
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
                            [ chr (shiftRightBy 11 a)
                            , chr (and (shiftRightBy 6 a) 31)
                            , chr (and (shiftRightBy 1 a) 31)
                            , chr (or (shiftLeftBy 4 (and a 1)) (shiftRightBy 4 b))
                            , chr (and (shiftRightBy 3 b) 30)
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
                            [ chr (shiftRightBy 11 a)
                            , chr (and (shiftRightBy 6 a) 31)
                            , chr (and (shiftRightBy 1 a) 31)
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
                            [ chr (shiftRightBy 3 a)
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
        Char.fromCode (n + 48)

    else
        Char.fromCode (n + 55)
