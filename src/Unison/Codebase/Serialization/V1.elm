module Unison.Codebase.Serialization.V1 exposing (..)

import Bytes exposing (..)
import Bytes.Decode exposing (..)
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


{-| FIXME
-}
type alias Word64 =
    ()


{-| FIXME
-}
type alias Int64 =
    ()


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


branchStarDecoder : Decoder (Branch.Star Referent NameSegment)
branchStarDecoder =
    Debug.todo ""


bytesDecoder : Int -> Decoder Bytes
bytesDecoder =
    Debug.todo ""


charDecoder : Decoder Char
charDecoder =
    Debug.todo ""


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
    Debug.todo ""


hashDecoder : Decoder Hash
hashDecoder =
    andThen bytesDecoder lengthDecoder


hash32Decoder : Decoder Hash32
hash32Decoder =
    map encodeHash hashDecoder


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
    Debug.todo ""


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
    Debug.todo ""


listDecoder : Decoder a -> Decoder (List a)
listDecoder decoder =
    andThen
        (\n -> replicate n decoder)
        lengthDecoder


mapDecoder : Decoder k -> Decoder v -> Decoder (List ( k, v ))
mapDecoder keyDecoder valDecoder =
    listDecoder (map2 Tuple.pair keyDecoder valDecoder)


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
    Debug.todo ""


patchDecoder : Decoder Patch
patchDecoder =
    map2
        Patch
        (relationDecoder referenceDecoder termEditDecoder)
        (relationDecoder referenceDecoder typeEditDecoder)


patternDecoder : Decoder Pattern
patternDecoder =
    Debug.todo ""


rawBranchDecoder : Decoder RawBranch
rawBranchDecoder =
    Debug.todo ""


rawCausalDecoder : Decoder RawCausal
rawCausalDecoder =
    Debug.todo ""


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

                _ ->
                    fail


relationDecoder : Decoder a -> Decoder b -> Decoder (Relation a b)
relationDecoder =
    Debug.todo ""


seqOpDecoder : Decoder SeqOp
seqOpDecoder =
    Debug.todo ""


symbolDecoder : Decoder Symbol
symbolDecoder =
    Debug.todo ""


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
    Debug.todo ""


typeDecoder : Decoder Type
typeDecoder =
    Debug.todo ""


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


{-| Run a decoder N times.
-}
replicate : Int -> Decoder a -> Decoder (List a)
replicate =
    Debug.todo ""


{-| Helper decoder that first decodes a one-byte tag.
-}
tagged : (Int -> Decoder a) -> Decoder a
tagged f =
    andThen f unsignedInt8
