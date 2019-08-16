module Serialization

import Bytes.Decode as Decode exposing (Decoder, andThen)

type DecodeError
  = UnknownTag String Int {- Word8 -}

type SeqOp = Cons | Snoc | Concat

type Int64
  = Int64_1 Int -- less than 32 bits
  | Int64_2 Int Int -- greater than 64 bits

type VarInt
  = Debug.Todo ""

type Hash
  = Debug.Todo ""


{-
unknownTag cannot be implemented due to no error handling in Bytes.Decode
-}


seqOpDecoder : Decoder SeqOp
seqOpDecoder = Decode.unsignedInt8 andThen
  (\n -> case n of
    0 -> Decoder Cons
    1 -> Decoder Snoc
    2 -> Decoder Concat)

unsignedInt64 : Int64
unsignedInt64 = Decode.map2
  (\a b -> case a of
    0 -> Int64_1 b
    _ -> Int64_2 a b)
  (Decode.unSignedInt32)
  (Decode.unSignedInt32)


booleanDecoder : Decoder Bool
booleanDecoder = Decoder Decode.unsignedInt8 andThen
  (\n -> case n of
    0 -> Decoder False
    1 -> Decoder True
    _ -> Debug.todo "" --no unknownTag handling
  )

intDecoder : Decoder Int64
intDecoder = Decoder unsignedInt64

varIntDecoder = Decoder VarInt

floatDecoder : Decoder Float
floatDecoder = Decode.Float64

textDecoder : Decoder Text
textDecoder = Debug.Todo ""

hashDecoder : Decoder Hash
