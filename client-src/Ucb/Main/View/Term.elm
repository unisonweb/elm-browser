module Ucb.Main.View.Term exposing (viewTerm)

import Array
import Element exposing (..)
import Int64 exposing (..)
import Ucb.Main.Model exposing (..)
import Ucb.Main.View.Reference exposing (viewReference)
import Ucb.Main.View.Type exposing (viewType)
import Ucb.Util.Pretty exposing (..)
import Unison.Symbol exposing (..)
import Unison.Term exposing (..)
import Word64 exposing (..)


type BlockContext
    = Block
    | Normal


type InfixContext
    = Infix
    | NonInfix


viewTerm :
    Model
    -> Term Symbol
    -> Element message
viewTerm model =
    viewTerm2
        { model = model
        , precedence = -1
        , blockContext = Normal
        , infixContext = NonInfix
        }


viewTerm2 :
    { model : Model
    , precedence : Int
    , blockContext : BlockContext
    , infixContext : InfixContext
    }
    -> Term Symbol
    -> Element message
viewTerm2 env { out } =
    case out of
        TermVar var ->
            text (symbolToString var)

        TermAbs var term ->
            text "(not implemented: TermAbs)"

        TermTm (TermInt n) ->
            text (String.fromInt (unsafeInt64ToInt53 n))

        TermTm (TermNat n) ->
            text (String.fromInt (unsafeWord64ToInt53 n))

        TermTm (TermFloat n) ->
            text (String.fromFloat n)

        TermTm (TermBoolean b) ->
            text
                (if b then
                    "true"

                 else
                    "false"
                )

        TermTm (TermSequence terms) ->
            row
                []
                [ text "["
                , row
                    []
                    (terms
                        |> Array.map
                            (viewTerm2
                                { model = env.model
                                , precedence = 0
                                , blockContext = Normal
                                , infixContext = NonInfix
                                }
                            )
                        |> Array.toList
                        |> List.intersperse (text ", ")
                    )
                , text "]"
                ]

        TermTm (TermText s) ->
            text "(not implemented: TermText)"

        TermTm (TermChar c) ->
            text "(not implemented: TermChar)"

        TermTm (TermBlank blank) ->
            text "(not implemented: TermBlank)"

        TermTm (TermRef reference) ->
            text "(not implemented: TermRef)"

        TermTm (TermConstructor reference n) ->
            text "(not implemented: TermConstructor)"

        TermTm (TermRequest reference n) ->
            text "(not implemented: TermRequest)"

        TermTm (TermHandle t1 t2) ->
            text "(not implemented: TermHandle)"

        TermTm (TermApp t1 t2) ->
            text "(not implemented: TermApp)"

        TermTm (TermAnn term type_) ->
            ppParen
                (env.precedence >= 0)
                (row
                    []
                    [ viewTerm2
                        { model = env.model
                        , precedence = 0
                        , blockContext = Normal
                        , infixContext = NonInfix
                        }
                        term
                    , text " : "
                    , viewType env.model 0 type_
                    ]
                )

        TermTm (TermIf t1 t2 t3) ->
            text "(not implemented: TermIf)"

        TermTm (TermAnd t1 t2) ->
            text "(not implemented: TermAnd)"

        TermTm (TermOr t1 t2) ->
            text "(not implemented: TermOr)"

        TermTm (TermLam term) ->
            text "(not implemented: TermLam)"

        TermTm (TermLetRec _ _ _) ->
            text "(not implemented: TermLetRec)"

        TermTm (TermLet _ _ _) ->
            text "(not implemented: TermLet)"

        TermTm (TermMatch _ _) ->
            text "(not implemented: TermMatch)"

        TermCycle _ ->
            text "(not implemented: TypeCycle)"
