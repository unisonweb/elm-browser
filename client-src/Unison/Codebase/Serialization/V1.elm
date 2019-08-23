module Unison.Codebase.Serialization.V1 exposing (..)

import Array
import Bitwise
import Bytes exposing (..)
import Bytes.Decode exposing (..)
import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Int64 exposing (..)
import Misc exposing (pairHashing, unsafeIndex)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing exposing (Hashing)
import Unison.ABT exposing (..)
import Unison.Codebase.Branch as Branch exposing (RawBranch)
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.Metadata exposing (..)
import Unison.Codebase.NameSegment exposing (..)
import Unison.Codebase.Patch exposing (..)
import Unison.Codebase.TermEdit exposing (..)
import Unison.Codebase.TypeEdit exposing (..)
import Unison.ConstructorType exposing (..)
import Unison.Declaration exposing (..)
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
import Unison.Var exposing (..)
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


dataDeclarationDecoder : Decoder (DataDeclaration Symbol)
dataDeclarationDecoder =
    map3
        DataDeclaration
        modifierDecoder
        (listDecoder symbolDecoder)
        (listDecoder (map2 Tuple.pair symbolDecoder typeDecoder))


declarationDecoder : Decoder (Declaration Symbol)
declarationDecoder =
    tagged <|
        \n ->
            case n of
                0 ->
                    map EffectDecl dataDeclarationDecoder

                1 ->
                    map DataDecl dataDeclarationDecoder

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


modifierDecoder : Decoder Modifier
modifierDecoder =
    tagged <|
        \n ->
            case n of
                0 ->
                    succeed Structural

                1 ->
                    map Unique textDecoder

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
    tagged <|
        \n ->
            case n of
                0 ->
                    succeed UnboundP

                1 ->
                    succeed VarP

                2 ->
                    map BooleanP booleanDecoder

                3 ->
                    map IntP intDecoder

                4 ->
                    map NatP natDecoder

                5 ->
                    map FloatP floatDecoder

                6 ->
                    map3
                        ConstructorP
                        referenceDecoder
                        varIntDecoder
                        (listDecoder patternDecoder)

                7 ->
                    map AsP patternDecoder

                8 ->
                    map EffectPureP patternDecoder

                9 ->
                    map4
                        EffectBindP
                        referenceDecoder
                        varIntDecoder
                        (listDecoder patternDecoder)
                        patternDecoder

                10 ->
                    map SequenceLiteralP (listDecoder patternDecoder)

                11 ->
                    map3 SequenceOpP patternDecoder seqOpDecoder patternDecoder

                12 ->
                    map TextP textDecoder

                13 ->
                    map CharP charDecoder

                _ ->
                    fail


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
            hash32Decoder
        )
        (hashDictDecoder
            nameSegmentEquality
            nameSegmentHashing
            nameSegmentDecoder
            hash32Decoder
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
    map2
        Symbol
        -- Fixme, is varint
        (map SmallWord64 varIntDecoder)
        (map User textDecoder)


termDecoder : Decoder (Term Symbol)
termDecoder =
    listDecoder symbolDecoder
        |> andThen (termDecoder2 [])


termDecoder2 :
    List Symbol
    -> List Symbol
    -> Decoder (Term Symbol)
termDecoder2 env fvs =
    tagged <|
        \n ->
            case n of
                0 ->
                    termVarDecoder env fvs

                1 ->
                    map
                        (termTerm symbolEquality symbolHashing)
                        (termFDecoder (termDecoder2 env fvs))

                2 ->
                    symbolDecoder
                        |> andThen
                            (\var ->
                                termDecoder2 (var :: env) fvs
                                    |> andThen
                                        (\body ->
                                            succeed (termAbs var body)
                                        )
                            )

                3 ->
                    map
                        termCycle
                        (termDecoder2 env fvs)

                _ ->
                    fail


termVarDecoder :
    List Symbol
    -> List Symbol
    -> Decoder (Term Symbol)
termVarDecoder env fvs =
    tagged <|
        \m ->
            case m of
                0 ->
                    map
                        (\i ->
                            termVar
                                symbolEquality
                                symbolHashing
                                (unsafeIndex i env)
                        )
                        varIntDecoder

                1 ->
                    map
                        (\i ->
                            termVar
                                symbolEquality
                                symbolHashing
                                (unsafeIndex i fvs)
                        )
                        varIntDecoder

                _ ->
                    fail


termFDecoder :
    Decoder (Term Symbol)
    -> Decoder (TermF Symbol)
termFDecoder tmDecoder =
    tagged <|
        \n ->
            case n of
                0 ->
                    map TermInt intDecoder

                1 ->
                    map TermNat natDecoder

                2 ->
                    map TermFloat floatDecoder

                3 ->
                    map TermBoolean booleanDecoder

                4 ->
                    map TermText textDecoder

                5 ->
                    map TermRef referenceDecoder

                6 ->
                    map2 TermConstructor referenceDecoder varIntDecoder

                7 ->
                    map2 TermRequest referenceDecoder varIntDecoder

                8 ->
                    map2 TermHandle tmDecoder tmDecoder

                9 ->
                    map2 TermApp tmDecoder tmDecoder

                10 ->
                    map2 TermAnn tmDecoder typeDecoder

                11 ->
                    map (Array.fromList >> TermSequence) (listDecoder tmDecoder)

                12 ->
                    map3 TermIf tmDecoder tmDecoder tmDecoder

                13 ->
                    map2 TermAnd tmDecoder tmDecoder

                14 ->
                    map2 TermOr tmDecoder tmDecoder

                15 ->
                    map TermLam tmDecoder

                16 ->
                    map2 (TermLetRec False) (listDecoder tmDecoder) tmDecoder

                17 ->
                    map2 (TermLet False) tmDecoder tmDecoder

                18 ->
                    map2 TermMatch
                        tmDecoder
                        (listDecoder
                            (map3 MatchCase
                                patternDecoder
                                (maybeDecoder tmDecoder)
                                tmDecoder
                            )
                        )

                19 ->
                    map TermChar charDecoder

                _ ->
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
    varIntDecoder
        |> andThen string


typeDecoder : Decoder (Type Symbol)
typeDecoder =
    listDecoder symbolDecoder
        |> andThen (typeDecoder2 [])


typeDecoder2 :
    List Symbol
    -> List Symbol
    -> Decoder (Type Symbol)
typeDecoder2 env fvs =
    tagged <|
        \n ->
            case n of
                0 ->
                    typeVarDecoder env fvs

                1 ->
                    map
                        (typeTerm symbolEquality symbolHashing)
                        (typeFDecoder (typeDecoder2 env fvs))

                2 ->
                    symbolDecoder
                        |> andThen
                            (\var ->
                                typeDecoder2 (var :: env) fvs
                                    |> andThen
                                        (\body ->
                                            succeed (typeAbs var body)
                                        )
                            )

                3 ->
                    map
                        typeCycle
                        (typeDecoder2 env fvs)

                _ ->
                    fail


typeVarDecoder :
    List Symbol
    -> List Symbol
    -> Decoder (Type Symbol)
typeVarDecoder env fvs =
    tagged <|
        \m ->
            case m of
                0 ->
                    map
                        (\i ->
                            typeVar
                                symbolEquality
                                symbolHashing
                                (unsafeIndex i env)
                        )
                        varIntDecoder

                1 ->
                    map
                        (\i ->
                            typeVar
                                symbolEquality
                                symbolHashing
                                (unsafeIndex i fvs)
                        )
                        varIntDecoder

                _ ->
                    fail


typeFDecoder :
    Decoder (Type Symbol)
    -> Decoder (TypeF Symbol)
typeFDecoder tyDecoder =
    tagged <|
        \n ->
            case n of
                0 ->
                    map TypeRef referenceDecoder

                1 ->
                    map2 TypeArrow tyDecoder tyDecoder

                2 ->
                    map2 TypeAnn tyDecoder kindDecoder

                3 ->
                    map2 TypeApp tyDecoder tyDecoder

                4 ->
                    map2 TypeEffect tyDecoder tyDecoder

                5 ->
                    map TypeEffects (listDecoder tyDecoder)

                6 ->
                    map TypeForall tyDecoder

                7 ->
                    map TypeIntroOuter tyDecoder

                _ ->
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
