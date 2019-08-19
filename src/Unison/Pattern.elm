module Unison.Pattern exposing (..)

import Int64 exposing (Int64)
import Unison.Reference exposing (Reference)
import Word64 exposing (Word64)


{-| Haskell type: Unison.Pattern.Pattern
-}
type Pattern
    = UnboundP
    | VarP
    | BooleanP Bool
    | IntP Int64
    | NatP Word64
    | FloatP Float
    | TextP String
    | CharP Char
    | ConstructorP Reference Int (List Pattern)
    | AsP Pattern
    | EffectPureP Pattern
    | EffectBindP Reference Int (List Pattern) Pattern
    | SequenceLiteralP (List Pattern)
    | SequenceOpP Pattern SeqOp Pattern


{-| Haskell type: Unison.Pattern.SeqOp
-}
type SeqOp
    = Cons
    | Snoc
    | Concat
