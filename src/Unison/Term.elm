module Unison.Term exposing (..)

import Array exposing (Array)
import Int64 exposing (Int64)
import Typeclasses.Classes.Equality exposing (Equality)
import Typeclasses.Classes.Hashing exposing (Hashing)
import Unison.ABT exposing (..)
import Unison.Blank exposing (Blank)
import Unison.Pattern exposing (Pattern)
import Unison.Reference exposing (Reference)
import Unison.Type exposing (Type)
import Word64 exposing (Word64)


{-| Haskell type: Unison.Term.Term
-}
type alias Term var =
    AbtTerm var (TermTerm var)


type TermTerm var
    = TermVar var
    | TermCycle (Term var)
    | TermAbs var (Term var)
    | TermInt Int64
    | TermNat Word64
    | TermFloat Float
    | TermBoolean Bool
    | TermText String
    | TermChar Char
    | TermBlank Blank
    | TermRef Reference
    | TermConstructor Reference Int
    | TermRequest Reference Int
    | TermHandle (Term var) (Term var)
    | TermApp (Term var) (Term var)
    | TermAnn (Term var) (Type var)
    | TermSequence (Array (Term var))
    | TermIf (Term var) (Term var) (Term var)
    | TermAnd (Term var) (Term var)
    | TermOr (Term var) (Term var)
    | TermLam (Term var)
    | TermLetRec Bool (List (Term var)) (Term var)
    | TermLet Bool (Term var) (Term var)
    | TermMatch (Term var) (List (MatchCase (Term var)))


{-| Haskell type: Unison.Term.MatchCase
-}
type MatchCase a
    = MatchCase Pattern (Maybe a) a


termVar :
    Equality var
    -> Hashing var
    -> var
    -> Term var
termVar varEquality varHashing =
    abtVar varEquality varHashing TermVar


termAbs :
    var
    -> Term var
    -> Term var
termAbs =
    abtAbs TermAbs


termCycle :
    Term var
    -> Term var
termCycle =
    abtCycle TermCycle
