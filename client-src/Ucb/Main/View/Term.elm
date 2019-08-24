module Ucb.Main.View.Term exposing (viewTerm)

import Element exposing (..)
import Unison.Symbol exposing (Symbol)
import Unison.Term exposing (Term)


viewTerm :
    Term Symbol
    -> Element message
viewTerm =
    Debug.toString >> text



{-
   case out of
       TermVar var ->
           viewSymbol var

       TermAbs var tm ->
           row
               [ spacing 2 ]
               [ viewSymbol var
               , text "->"
               , viewTerm tm
               ]

       TermTm (TermInt n) ->
           text "TermInt"

       TermTm (TermNat n) ->
           text (String.fromInt (unsafeWord64ToWord53 n))

       TermTm (TermFloat n) ->
           text (String.fromFloat n)

       TermTm (TermBoolean b) ->
           text
               (if b then
                   "true"

                else
                   "false"
               )

       TermTm (TermText s) ->
           -- TODO escape
           text ("\"" ++ s ++ "\"")

       TermTm (TermChar c) ->
           text ("'" ++ String.fromChar c ++ "'")

       -- | TermBlank Blank
       -- | TermRef Reference
       -- | TermConstructor Reference Int
       -- | TermRequest Reference Int
       -- | TermHandle (Term var) (Term var)
       -- | TermApp (Term var) (Term var)
       -- | TermAnn (Term var) (Type var)
       -- | TermSequence (Array (Term var))
       -- | TermIf (Term var) (Term var) (Term var)
       -- | TermAnd (Term var) (Term var)
       -- | TermOr (Term var) (Term var)
       -- | TermLam (Term var)
       -- | TermLetRec Bool (List (Term var)) (Term var)
       -- | TermLet Bool (Term var) (Term var)
       -- | TermMatch (Term var) (List (MatchCase (Term var)))
       TermCycle _ ->
           text "TermCycle"
-}
