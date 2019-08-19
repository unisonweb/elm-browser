module Unison.Pattern exposing (..)

import Int64 exposing (Int64)
import Unison.Reference exposing (Reference)
import Word64 exposing (Word64)


{-| Haskell type: Unison.Pattern.Pattern
-}
type Pattern ann
    = UnboundP ann
    | VarP ann
    | BooleanP ann Bool
    | IntP ann Int64
    | NatP ann Word64
    | FloatP ann Float
    | TextP ann String
    | CharP ann Char
    | ConstructorP ann Reference Int (List (Pattern ann))
    | AsP ann (Pattern ann)
    | EffectPureP ann (Pattern ann)
    | EffectBindP ann Reference Int (List (Pattern ann)) (Pattern ann)
    | SequenceLiteralP ann (List (Pattern ann))
    | SequenceOpP ann (Pattern ann) SeqOp (Pattern ann)


{-| Haskell type: Unison.Pattern.SeqOp
-}
type SeqOp
    = Cons
    | Snoc
    | Concat
