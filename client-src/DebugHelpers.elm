-- | Debug stuff.


module DebugHelpers exposing (..)

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Misc exposing (emptyBytes)


run : Bytes.Decode.Decoder a -> String -> Maybe a
run d b =
    Bytes.Decode.decode d (hex b)


{-| "01" -> 0x01
-}
hex : String -> Bytes
hex =
    readHexString
        >> List.map Bytes.Encode.unsignedInt8
        >> Bytes.Encode.sequence
        >> Bytes.Encode.encode


{-| "01FF" -> [1,255]
-}
readHexString : String -> List Int
readHexString =
    let
        hex1 : Char -> Maybe Int
        hex1 c =
            if c >= '0' && c <= '9' then
                Just (Char.toCode c - 48)

            else if c >= 'a' && c <= 'f' then
                Just (Char.toCode c - 87)

            else if c >= 'A' && c <= 'F' then
                Just (Char.toCode c - 55)

            else
                Nothing

        hex2 : Char -> Char -> Maybe Int
        hex2 c1 c2 =
            Maybe.map2
                (\n1 n2 -> Bitwise.or (Bitwise.shiftLeftBy 4 n1) n2)
                (hex1 c1)
                (hex1 c2)

        hex_ : String -> List Int
        hex_ xs =
            case String.uncons xs of
                Nothing ->
                    []

                Just ( c1, ys ) ->
                    case String.uncons ys of
                        Nothing ->
                            []

                        Just ( c2, zs ) ->
                            case hex2 c1 c2 of
                                Nothing ->
                                    []

                                Just n ->
                                    n :: hex_ zs
    in
    hex_
