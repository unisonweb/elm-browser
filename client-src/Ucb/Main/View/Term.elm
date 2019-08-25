module Ucb.Main.View.Term exposing (viewTerm)

import Array
import Element exposing (..)
import Ucb.Main.Model exposing (..)
import Ucb.Main.View.Reference exposing (viewReference)
import Ucb.Main.View.Symbol exposing (viewSymbol)
import Ucb.Main.View.Type exposing (viewType)
import Unison.Symbol exposing (..)
import Unison.Term exposing (..)
import Word64 exposing (..)


viewTerm :
    Model
    -> Term Symbol
    -> Element message
viewTerm model { out } =
    case out of
        TermVar var ->
            viewSymbol var

        TermAbs var tm ->
            row
                [ spacing 2 ]
                [ viewSymbol var
                , text "->"
                , viewTerm model tm
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
            -- TODO escape double quotes
            text ("\"" ++ s ++ "\"")

        TermTm (TermChar c) ->
            text ("'" ++ String.fromChar c ++ "'")

        TermTm (TermBlank blank) ->
            text (Debug.toString blank)

        TermTm (TermRef reference) ->
            viewReference { showBuiltin = True, take = Just 7 } reference

        TermTm (TermConstructor reference n) ->
            row
                []
                [ viewReference { showBuiltin = True, take = Just 7 } reference
                , text (String.cons '#' (String.fromInt n))
                ]

        TermTm (TermRequest reference n) ->
            row
                []
                [ viewReference { showBuiltin = True, take = Just 7 } reference
                , text (String.cons '#' (String.fromInt n))
                ]

        TermTm (TermHandle t1 t2) ->
            row
                [ spacing 2 ]
                [ text "handle"
                , viewTerm model t1
                , text "in"
                , viewTerm model t2
                ]

        TermTm (TermApp t1 t2) ->
            row
                [ spacing 2 ]
                [ viewTerm model t1
                , viewTerm model t2
                ]

        TermTm (TermAnn term type_) ->
            row
                [ spacing 2 ]
                [ viewTerm model term
                , text ":"
                , viewType model type_
                ]

        TermTm (TermSequence terms) ->
            row
                []
                [ text "["
                , row []
                    (terms
                        |> Array.toList
                        |> List.map (viewTerm model)
                        |> List.intersperse (text ", ")
                    )
                , text "]"
                ]

        TermTm (TermIf t1 t2 t3) ->
            row
                [ spacing 2 ]
                [ text "if"
                , viewTerm model t1
                , text "then"
                , viewTerm model t2
                , text "else"
                , viewTerm model t3
                ]

        TermTm (TermAnd t1 t2) ->
            row
                [ spacing 2 ]
                [ text "and"
                , viewTerm model t1
                , viewTerm model t2
                ]

        TermTm (TermOr t1 t2) ->
            row
                [ spacing 2 ]
                [ text "or"
                , viewTerm model t1
                , viewTerm model t2
                ]

        TermTm (TermLam term) ->
            row
                [ spacing 2 ]
                [ text "("
                , text "λ"
                , viewTerm model term
                , text ")"
                ]

        TermTm (TermLetRec _ _ _) ->
            text "TermLetRec"

        TermTm (TermLet _ _ _) ->
            text "TermLet"

        TermTm (TermMatch _ _) ->
            text "TermMatch"

        TermCycle _ ->
            text "TermCycle"
