module Unison.Codebase.Serialization.V1 exposing (..)

import Bitwise
import Bytes exposing (..)
import Bytes.Decode exposing (..)
import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Int64 exposing (..)
import Misc exposing (pairHashing)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
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
import Unison.Term exposing (..)
import Unison.Type exposing (..)
import Unison.Util.Relation exposing (..)
import Unison.Util.Star3 exposing (..)
import Word64 exposing (..)


booleanDecoder : Decoder Bool
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
    -> Equality b
    -> Hashing b
    -> Decoder a
    -> Decoder b
    -> Decoder (Branch.Star a b)
branchStarDecoder equalityA hashingA equalityB hashingB decoderA decoderB =
    map4
        Star3_
        (map
            (HashSet.fromList equalityA hashingA)
            (listDecoder decoderA)
        )
        (relationDecoder
            equalityA
            hashingA
            equalityB
            hashingB
            decoderA
            decoderB
        )
        (relationDecoder
            equalityA
            hashingA
            referenceEquality
            referenceHashing
            decoderA
            referenceDecoder
        )
        (relationDecoder
            equalityA
            hashingA
            (Equality.tuple2 referenceEquality referenceEquality)
            (pairHashing referenceHashing referenceHashing)
            decoderA
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

                1 ->
                    succeed Effect

                _ ->
                    fail


floatDecoder : Decoder Float
floatDecoder =
    float64 BE


hashDecoder : Decoder Hash
hashDecoder =
    varIntDecoder
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
    map2 intsToInt64 (unsignedInt32 BE) (unsignedInt32 BE)


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


listDecoder : Decoder a -> Decoder (List a)
listDecoder decoder =
    varIntDecoder
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
        (relationDecoder
            referenceEquality
            referenceHashing
            termEditEquality
            termEditHashing
            referenceDecoder
            termEditDecoder
        )
        (relationDecoder
            referenceEquality
            referenceHashing
            typeEditEquality
            typeEditHashing
            referenceDecoder
            typeEditDecoder
        )


patternDecoder : Decoder Pattern
patternDecoder =
    Debug.todo "patternDecoder"


rawBranchDecoder : Decoder RawBranch
rawBranchDecoder =
    map4
        RawBranch
        (branchStarDecoder
            referentEquality
            referentHashing
            nameSegmentEquality
            nameSegmentHashing
            referentDecoder
            nameSegmentDecoder
        )
        (branchStarDecoder
            referenceEquality
            referenceHashing
            nameSegmentEquality
            nameSegmentHashing
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
                    map Derived (map3 Id hash32Decoder varIntDecoder varIntDecoder)

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
                    map3 Con referenceDecoder varIntDecoder constructorTypeDecoder

                _ ->
                    fail


relationDecoder :
    Equality a
    -> Hashing a
    -> Equality b
    -> Hashing b
    -> Decoder a
    -> Decoder b
    -> Decoder (Relation a b)
relationDecoder equalityA hashingA equalityB hashingB decoderA decoderB =
    map
        (relationFromList
            equalityA
            hashingA
            equalityB
            hashingB
        )
        (listDecoder (map2 Tuple.pair decoderA decoderB))


seqOpDecoder : Decoder SeqOp
seqOpDecoder =
    tagged <|
        \n ->
            case n of
                0 ->
                    succeed Cons

                1 ->
                    succeed Snoc

                2 ->
                    succeed Concat

                _ ->
                    fail


symbolDecoder : Decoder Symbol
symbolDecoder =
    Debug.todo "symbolDecoder"


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
    varIntDecoder
        |> andThen string


typeDecoder : Decoder Type
typeDecoder =
    Debug.todo "typeDecoder"


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
                    varIntDecoder
                        |> andThen
                            (\m ->
                                succeed
                                    (Bitwise.or
                                        (Bitwise.shiftLeftBy 7 m)
                                        (Bitwise.and n 127)
                                    )
                            )

                else
                    succeed n
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
