module Unison.Codebase.Serialization.V1 exposing (..)

import Bytes exposing (..)
import Bytes.Decode exposing (Decoder)
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
    Debug.todo ""


branchStarDecoder : Decoder (Branch.Star ReferentOrdering NameSegment Referent NameSegment)
branchStarDecoder =
    Debug.todo ""


charDecoder : Decoder Char
charDecoder =
    Debug.todo ""


constructorTypeDecoder : Decoder ConstructorType
constructorTypeDecoder =
    Debug.todo ""


floatDecoder : Decoder Float
floatDecoder =
    Debug.todo ""


hashDecoder : Decoder Hash
hashDecoder =
    Debug.todo ""


eitherDecoder : Decoder a -> Decoder b -> Decoder { left : a, right : b }
eitherDecoder =
    Debug.todo ""


intDecoder : Decoder Int64
intDecoder =
    Debug.todo ""


kindDecoder : Decoder Kind
kindDecoder =
    Debug.todo ""


listDecoder : Decoder a -> Decoder (List a)
listDecoder =
    Debug.todo ""


mapDecoder : Decoder k -> Decoder v -> Decoder (List ( k, v ))
mapDecoder =
    Debug.todo ""


maybeDecoder : Decoder a -> Decoder (Maybe a)
maybeDecoder =
    Debug.todo ""


metadataTypeDecoder : Decoder MetadataType
metadataTypeDecoder =
    Debug.todo ""


metadataValueDecoder : Decoder MetadataValue
metadataValueDecoder =
    Debug.todo ""


nameSegmentDecoder : Decoder NameSegment
nameSegmentDecoder =
    Debug.todo ""


natDecoder : Decoder Word64
natDecoder =
    Debug.todo ""


patchDecoder : Decoder Patch
patchDecoder =
    Debug.todo ""


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
    Debug.todo ""


referentDecoder : Decoder Referent
referentDecoder =
    Debug.todo ""


seqOpDecoder : Decoder SeqOp
seqOpDecoder =
    Debug.todo ""


symbolDecoder : Decoder Symbol
symbolDecoder =
    Debug.todo ""


termEditDecoder : Decoder TermEdit
termEditDecoder =
    Debug.todo ""


textDecoder : Decoder String
textDecoder =
    Debug.todo ""


typeDecoder : Decoder Type
typeDecoder =
    Debug.todo ""


typeEditDecoder : Decoder TypeEdit
typeEditDecoder =
    Debug.todo ""
