module Unison.Term exposing (..)

import Array exposing (Array)
import Int64 exposing (Int64)
import Unison.ABT exposing (..)
import Unison.Blank exposing (Blank)
import Unison.Pattern exposing (Pattern)
import Unison.Reference exposing (Reference)
import Unison.Type exposing (Type)
import Word64 exposing (Word64)


{-| Haskell type: Unison.Term.Term
-}
type alias Term var ann =
    AbtTerm var ann (TermTerm var ann)


type TermTerm var ann
    = TermVar var
    | TermCycle (Term var ann)
    | TermAbs var (Term var ann)
    | TermInt Int64
    | TermWord Word64
    | TermFloat Float
    | TermBoolean Bool
    | TermText String
    | TermChar Char
    | TermBlank (Blank ann)
    | TermRef Reference
    | TermConstructor Reference Int
    | TermRequest Reference Int
    | TermHandle (Term var ann) (Term var ann)
    | TermApp (Term var ann) (Term var ann)
    | TermAnn (Term var ann) (Type var ann)
    | TermSequence (Array (Term var ann))
    | TermIf (Term var ann) (Term var ann) (Term var ann)
    | TermAnd (Term var ann) (Term var ann)
    | TermOr (Term var ann) (Term var ann)
    | TermLam (Term var ann)
    | TermLetRec Bool (List (Term var ann)) (Term var ann)
    | TermLet Bool (Term var ann) (Term var ann)
    | TermMatch (Term var ann) (List (MatchCase ann (Term var ann)))


{-| Haskell type: Unison.Term.MatchCase
-}
type MatchCase ann a
    = MatchCase (Pattern ann) (Maybe a) a
