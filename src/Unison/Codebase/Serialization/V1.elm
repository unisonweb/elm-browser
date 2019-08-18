module Unison.Codebase.Serialization.V1 exposing (decodeRawCausal)

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


decodeRawCausal : Bytes -> Maybe RawCausal
decodeRawCausal =
    decode rawCausalDecoder



--------------------------------------------------------------------------------
-- Decoders
--------------------------------------------------------------------------------


booleanDecoder : Decoder Bool
booleanDecoder =
    Debug.log "boolean" <|
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
    Debug.log "branchStar" <|
        map4
            Star3_
            (map
                (Debug.log "calling HashSet.fromList" <| HashSet.fromList equalityA hashingA)
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
    Debug.log "char" <|
        map Char.fromCode varIntDecoder


constructorTypeDecoder : Decoder ConstructorType
constructorTypeDecoder =
    Debug.log "constructorType" <|
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
    Debug.log "float" <|
        float64 BE


hashDecoder : Decoder Hash
hashDecoder =
    Debug.log "hash" <|
        (varIntDecoder
            |> andThen
                (\n ->
                    Debug.log
                        ("trying to parse " ++ Debug.toString n ++ " bytes")
                        (bytes n)
                )
        )


hash32Decoder : Decoder Hash32
hash32Decoder =
    Debug.log "hash32" <|
        map encodeHash hashDecoder


hashDictDecoder :
    Equality k
    -> Hashing k
    -> Decoder k
    -> Decoder v
    -> Decoder (HashDict k v)
hashDictDecoder keyEquality keyHashing keyDecoder valDecoder =
    Debug.log "hash32" <|
        map
            (HashDict.fromList keyEquality keyHashing)
            (listDecoder (map2 Tuple.pair keyDecoder valDecoder))


hashSetDecoder :
    Equality a
    -> Hashing a
    -> Decoder a
    -> Decoder (HashSet a)
hashSetDecoder equality hashing decoder =
    Debug.log "hashSet" <|
        map
            (HashSet.fromList equality hashing)
            (listDecoder decoder)


eitherDecoder : Decoder a -> Decoder b -> Decoder (Result a b)
eitherDecoder leftDecoder rightDecoder =
    Debug.log "either" <|
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
    Debug.log "int" <|
        map2 intsToInt64 (unsignedInt32 BE) (unsignedInt32 BE)


kindDecoder : Decoder Kind
kindDecoder =
    Debug.log "kind" <|
        (tagged <|
            \n ->
                case n of
                    0 ->
                        succeed Star

                    1 ->
                        map2 Arrow kindDecoder kindDecoder

                    _ ->
                        fail
        )


listDecoder : Decoder a -> Decoder (List a)
listDecoder decoder =
    Debug.log "list" <|
        (varIntDecoder
            |> andThen
                (\n -> Debug.log ("I'm about to call replicate with " ++ Debug.toString n) (replicate n decoder))
        )


maybeDecoder : Decoder a -> Decoder (Maybe a)
maybeDecoder decoder =
    Debug.log "maybe" <|
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
    Debug.log "text" <|
        textDecoder


natDecoder : Decoder Word64
natDecoder =
    Debug.log "nat" <|
        map2
            intsToWord64
            (unsignedInt32 BE)
            (unsignedInt32 BE)


patchDecoder : Decoder Patch
patchDecoder =
    Debug.log "patch" <|
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
    Debug.log "rawBranch" <|
        map4
            RawBranch
            (Debug.log "rawBranchDecoder.branchStarDecoder" <|
                branchStarDecoder
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
    Debug.log "rawCausal"
        (tagged <|
            \n ->
                case n of
                    0 ->
                        Debug.log "rawCausal.0" <|
                            map RawOne rawBranchDecoder

                    1 ->
                        Debug.log "rawCausal.1" <|
                            map2
                                (\hash branch -> RawCons branch hash)
                                hash32Decoder
                                (Debug.log "entering rawBranchDecoder" rawBranchDecoder)

                    2 ->
                        Debug.log "rawCausal.2" <|
                            map2
                                (\hashes branch -> RawMerge branch hashes)
                                (hashSetDecoder hash32Equality hash32Hashing hash32Decoder)
                                rawBranchDecoder

                    _ ->
                        Debug.log "rawCausal fail" <|
                            fail
        )


referenceDecoder : Decoder Reference
referenceDecoder =
    Debug.log "reference" <|
        tagged <|
            \n ->
                case n of
                    0 ->
                        Debug.log "reference.0" <|
                            map Builtin textDecoder

                    1 ->
                        Debug.log "reference.1" <|
                            map Derived (map3 Id hash32Decoder varIntDecoder varIntDecoder)

                    _ ->
                        Debug.log "reference fail" <|
                            fail


referentDecoder : Decoder Referent
referentDecoder =
    Debug.log "referent" <|
        tagged <|
            \n ->
                case n of
                    0 ->
                        Debug.log "referent.0" <|
                            map Ref referenceDecoder

                    1 ->
                        Debug.log "referent.1" <|
                            map3 Con referenceDecoder varIntDecoder constructorTypeDecoder

                    _ ->
                        Debug.log "referent fail" <|
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
    Debug.log "seqOp" <|
        tagged <|
            \n ->
                case n of
                    0 ->
                        Debug.log "seqOp.0" <|
                            succeed Cons

                    1 ->
                        Debug.log "seqOp.1" <|
                            succeed Snoc

                    2 ->
                        Debug.log "seqOp.2" <|
                            succeed Concat

                    _ ->
                        Debug.log "seqOp fail" <|
                            fail


symbolDecoder : Decoder Symbol
symbolDecoder =
    Debug.todo "symbolDecoder"


termEditDecoder : Decoder TermEdit
termEditDecoder =
    Debug.log "termEdit" <|
        tagged <|
            \n ->
                case n of
                    1 ->
                        Debug.log "termEdit.1" <|
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
                        Debug.log "termEdit.2" <|
                            succeed TermEditDeprecate

                    _ ->
                        Debug.log "termEdit fail" <|
                            fail


textDecoder : Decoder String
textDecoder =
    Debug.log "text" <|
        (varIntDecoder
            |> andThen string
        )


typeDecoder : Decoder Type
typeDecoder =
    Debug.todo "typeDecoder"


typeEditDecoder : Decoder TypeEdit
typeEditDecoder =
    Debug.log "typeEdit" <|
        tagged <|
            \n ->
                case n of
                    1 ->
                        Debug.log "typeEdit.1" <|
                            map TypeEditReplace referenceDecoder

                    2 ->
                        Debug.log "typeEdit.2" <|
                            succeed TypeEditDeprecate

                    _ ->
                        Debug.log "typeEdit fail" <|
                            fail


{-| TODO This is busted for ints that can't fit into JS nums, the actual type
should return Int64. But I (mitchell) don't feel like writing bit-fiddly code
right now.
-}
varIntDecoder : Decoder Int
varIntDecoder =
    Debug.log "varInt" <|
        (unsignedInt8
            |> andThen
                (\n ->
                    if Bitwise.and n 128 == 128 then
                        case Debug.log ("found byte 8th bit is set: " ++ Debug.toString n ++ ", returning it") () of
                            () ->
                                varIntDecoder
                                    |> andThen
                                        (\m ->
                                            let
                                                xxx =
                                                    Bitwise.or
                                                        (Bitwise.shiftLeftBy 7 m)
                                                        (Bitwise.and n 127)
                                            in
                                            Debug.log ("succeeding varInt with " ++ Debug.toString xxx)
                                                (succeed xxx)
                                        )

                    else
                        case Debug.log ("found byte without 8th bit set: " ++ Debug.toString n ++ ", I'm done!") () of
                            () ->
                                succeed n
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
        Debug.log "replicate: succeed []" (succeed [])

    else
        map2 (::) decoder (replicate (n - 1) decoder)


{-| Helper decoder that first decodes a one-byte tag.
-}
tagged : (Int -> Decoder a) -> Decoder a
tagged f =
    unsignedInt8
        |> andThen f
