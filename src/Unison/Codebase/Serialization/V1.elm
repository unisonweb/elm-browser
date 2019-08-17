module Unison.Codebase.Serialization.V1 exposing (decodeRawCausal)

import Bitwise
import Bytes exposing (..)
import Bytes.Decode exposing (..)
import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Int64 exposing (..)
import Typeclasses.Classes.Equality exposing (Equality)
import Typeclasses.Classes.Hashing exposing (Hashing)
import Unison.Codebase.Branch as Branch exposing (RawBranch)
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.Metadata exposing (..)
import Unison.Codebase.NameSegment exposing (..)
import Unison.Codebase.Patch exposing (..)
import Unison.Codebase.TermEdit exposing (..)
import Unison.Codebase.TypeEdit exposing (..)
import Unison.ConstructorType exposing (..)
import Unison.Hash exposing (..)
import Unison.Kind exposing (..)
import Unison.Pattern exposing (..)
import Unison.Reference exposing (..)
import Unison.Referent exposing (..)
import Unison.Symbol exposing (..)
import Unison.Type exposing (..)
import Unison.Util.Relation exposing (..)
import Unison.Util.Star3 exposing (..)
import Word64 exposing (..)


decodeRawCausal : Bytes -> Maybe RawCausal
decodeRawCausal =
    decode rawCausalDecoder



--------------------------------------------------------------------------------
-- Decoders
--------------------------------------------------------------------------------
booleanDecoder : Decoder Bool
<<<<<<< Updated upstream
booleanDecoder =
    tagged <|
        \n ->
            case n of
                0 ->
                    succeed False

                1 ->
                    succeed True

                _ ->
                    fail


branchStarDecoder :
    Equality a
    -> Hashing a
    -> Decoder a
    -> Decoder b
    -> Decoder (Branch.Star a b)
branchStarDecoder equalityA hashingA decoderA decoderB =
    map4
        Star3_
        (map
            (HashSet.fromList equalityA hashingA)
            (listDecoder decoderA)
        )
        (relationDecoder decoderA decoderB)
        (relationDecoder decoderA referenceDecoder)
        (relationDecoder decoderA
            (map2 Tuple.pair referenceDecoder referenceDecoder)
        )

charDecoder : Decoder Char
charDecoder =
    map Char.fromCode varIntDecoder


constructorTypeDecoder : Decoder ConstructorType
constructorTypeDecoder =
    tagged <|
        \n ->
            case n of
                0 ->
                    succeed Data
=======
booleanDecoder = Decode.unsignedInt8 
  |> Decode.andThen (\n -> case n of
    0 -> Decode.succeed False
    1 -> Decode.succeed True
    _ -> Debug.todo "unknown Bool"
  )

branchStarDecoder : Decoder (Branch.Star Referent NameSegment)
branchStarDecoder =
    Debug.todo ""


charDecoder : Decoder Char
charDecoder =
  


constructorTypeDecoder : Decoder ConstructorType
constructorTypeDecoder = Decode.unsignedInt8
  |> Decode.andThen (\n -> case n of
    0 -> Decode.succeed CT.Data
    1 -> Decode.succeed CT.Effect
    _ -> Debug.todo "unknown ConstructorType"
  )
>>>>>>> Stashed changes

                1 ->
                    succeed Effect

                _ ->
                    fail


floatDecoder : Decoder Float
floatDecoder =
    float64 BE

hashDecoder : Decoder Hash
hashDecoder =
    lengthDecoder
        |> andThen bytes


hash32Decoder : Decoder Hash32
hash32Decoder =
    map encodeHash hashDecoder


hashDictDecoder :
    Equality k
    -> Hashing k
    -> Decoder k
    -> Decoder v
    -> Decoder (HashDict k v)
hashDictDecoder keyEquality keyHashing keyDecoder valDecoder =
    map
        (HashDict.fromList keyEquality keyHashing)
        (listDecoder (map2 Tuple.pair keyDecoder valDecoder))

hashSetDecoder :
    Equality a
    -> Hashing a
    -> Decoder a
    -> Decoder (HashSet a)
hashSetDecoder equality hashing decoder =
    map
        (HashSet.fromList equality hashing)
        (listDecoder decoder)


eitherDecoder : Decoder a -> Decoder b -> Decoder (Result a b)
eitherDecoder leftDecoder rightDecoder =
    tagged <|
        \n ->
            case n of
                0 ->
                    map Err leftDecoder

                1 ->
                    map Ok rightDecoder

                _ ->
                    fail


intDecoder : Decoder Int64
intDecoder =
  map2 (intsToInt64) (unsignedInt32 BE) (unsignedInt32 BE)

<<<<<<< Updated upstream
kindDecoder : Decoder Kind
kindDecoder =
    tagged <|
        \n ->
            case n of
                0 ->
                    succeed Star
                1 ->
                    map2 Arrow kindDecoder kindDecoder
                _ ->
                    fail

lengthDecoder : Decoder Int
lengthDecoder =
    fail
=======
maybeDecoder : Decoder a -> Decoder (Maybe a)
maybeDecoder getA = Decode.unsignedInt8
  |> Decode.andThen (\n -> case n of
    0 -> Decode.succeed Nothing
    1 -> Decode.succeed (Just getA)
    _ -> Debug.todo "unknown Maybe"
  )
    
>>>>>>> Stashed changes


listDecoder : Decoder a -> Decoder (List a)
listDecoder decoder =
    lengthDecoder
        |> andThen
            (\n -> replicate n decoder)
          
maybeDecoder : Decoder a -> Decoder (Maybe a)
maybeDecoder decoder =
    tagged <|
        \n ->
            case n of
                0 ->
                    succeed Nothing

                1 ->
                    map Just decoder

                _ ->
                    fail


nameSegmentDecoder : Decoder NameSegment
nameSegmentDecoder =
    textDecoder


natDecoder : Decoder Word64
natDecoder =
    map2
        intsToWord64
        (unsignedInt32 BE)
        (unsignedInt32 BE)

patchDecoder : Decoder Patch
patchDecoder =
    map2
        Patch
        (relationDecoder referenceDecoder termEditDecoder)
        (relationDecoder referenceDecoder typeEditDecoder)


patternDecoder : Decoder Pattern
patternDecoder =
    fail

rawBranchDecoder : Decoder RawBranch
rawBranchDecoder =
    map4
        RawBranch
        (branchStarDecoder
            referentEquality
            referentHashing
            referentDecoder
            nameSegmentDecoder
        )
        (branchStarDecoder
            referenceEquality
            referenceHashing
            referenceDecoder
            nameSegmentDecoder
        )
        (hashDictDecoder
            nameSegmentEquality
            nameSegmentHashing
            nameSegmentDecoder
            hashDecoder
        )
        (hashDictDecoder
            nameSegmentEquality
            nameSegmentHashing
            nameSegmentDecoder
            hashDecoder
        )


rawCausalDecoder : Decoder RawCausal
rawCausalDecoder =
    tagged <|
        \n ->
            case n of
                0 ->
                    map RawOne rawBranchDecoder

                1 ->
                    map2
                        (\hash branch -> RawCons branch hash)
                        hash32Decoder
                        rawBranchDecoder

                2 ->
                    map2
                        (\hashes branch -> RawMerge branch hashes)
                        (hashSetDecoder hash32Equality hash32Hashing hash32Decoder)
                        rawBranchDecoder

                _ ->
                    fail


referenceDecoder : Decoder Reference
referenceDecoder =
    tagged <|
        \n ->
            case n of
                0 ->
                    map Builtin textDecoder

                1 ->
                    map Derived (map3 Id hash32Decoder lengthDecoder lengthDecoder)

                _ ->
                    fail


referentDecoder : Decoder Referent
referentDecoder =
    tagged <|
        \n ->
            case n of
                0 ->
                    map Ref referenceDecoder

                1 ->
                    map3 Con referenceDecoder lengthDecoder constructorTypeDecoder

<<<<<<< Updated upstream
                _ ->
                    fail
=======
seqOpDecoder : Decoder SeqOp
seqOpDecoder = Decode.unsignedInt8
  |> Decode.andThen (\n -> case n of
    0 -> Decode.succeed Cons
    1 -> Decode.succeed Snoc
    2 -> Decode.succed Concat
  )
>>>>>>> Stashed changes


relationDecoder : Decoder a -> Decoder b -> Decoder (Relation a b)
relationDecoder _ _ =
    fail


seqOpDecoder : Decoder SeqOp
seqOpDecoder = 
  tagged <| 
    \n -> 
      case n of
        0 -> succeed Cons
        1 -> succeed Snoc
        2 -> succeed Concat
        _ -> Debug.todo "unknown seqOp"
  
symbolDecoder : Decoder Symbol
symbolDecoder =
    fail


termEditDecoder : Decoder TermEdit
termEditDecoder =
    tagged <|
        \n ->
            case n of
                1 ->
                    map2
                        TermEditReplace
                        referenceDecoder
                        (tagged <|
                            \m ->
                                case m of
                                    1 ->
                                        succeed Same

                                    2 ->
                                        succeed Subtype

                                    3 ->
                                        succeed Different

                                    _ ->
                                        fail
                        )

                2 ->
                    succeed TermEditDeprecate

                _ ->
                    fail


textDecoder : Decoder String
textDecoder =
  lengthDecoder
  |> andThen string


typeDecoder : Decoder Type
typeDecoder =
    fail


typeEditDecoder : Decoder TypeEdit
typeEditDecoder =
    tagged <|
        \n ->
            case n of
                1 ->
                    map TypeEditReplace referenceDecoder

                2 ->
                    succeed TypeEditDeprecate

                _ ->
                    fail


{-| TODO This is busted for ints that can't fit into JS nums, the actual type
should return Int64. But I (mitchell) don't feel like writing bit-fiddly code
right now.
-}
varIntDecoder : Decoder Int
varIntDecoder =
    unsignedInt8
        |> andThen
            (\n ->
                if Bitwise.and n 128 == 128 then
                    succeed n

                else
                    varIntDecoder
                        |> andThen
                            (\m ->
                                succeed
                                    (Bitwise.or
                                        (Bitwise.shiftLeftBy 7 m)
                                        (Bitwise.and n 127)
                                    )
                            )
            )



--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------


{-| Run a decoder N times.
-}
replicate : Int -> Decoder a -> Decoder (List a)
replicate n decoder =
    if n == 0 then
        succeed []

    else
        map2 (::) decoder (replicate (n - 1) decoder)


{-| Helper decoder that first decodes a one-byte tag.
-}
tagged : (Int -> Decoder a) -> Decoder a
tagged f =
    unsignedInt8
        |> andThen f