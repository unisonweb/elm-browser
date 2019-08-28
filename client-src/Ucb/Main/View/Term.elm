module Ucb.Main.View.Term exposing (viewTerm)

import Array
import Element exposing (..)
import HashingContainers.HashDict as HashDict
import HashingContainers.HashSet as HashSet
import Int64 exposing (..)
import Misc exposing (..)
import Ucb.Main.Model exposing (..)
import Ucb.Main.View.Reference exposing (viewReference)
import Ucb.Main.View.Referent exposing (viewReferent)
import Ucb.Main.View.Type exposing (viewType)
import Ucb.Unison.Name exposing (..)
import Ucb.Unison.NameDict exposing (..)
import Ucb.Unison.ReferentSet exposing (..)
import Ucb.Util.Pretty exposing (..)
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.ConstructorType exposing (..)
import Unison.Name exposing (..)
import Unison.Reference exposing (..)
import Unison.Referent exposing (..)
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

        TermTm (TermConstructor reference n) ->
            viewConstructor env.model reference n

        TermTm (TermText s) ->
            text "(not implemented: TermText)"

        TermTm (TermChar c) ->
            text "(not implemented: TermChar)"

        TermTm (TermBlank blank) ->
            text "(not implemented: TermBlank)"

        TermTm (TermRef reference) ->
            text "(not implemented: TermRef)"

        TermTm (TermRequest reference n) ->
            text "(not implemented: TermRequest)"

        TermTm (TermHandle t1 t2) ->
            text "(not implemented: TermHandle)"

        TermTm (TermApp t1 t2) ->
            text "(not implemented: TermApp)"

        TermTm (TermAnn term type_) ->
            viewTerm2 env term

        {-
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
        -}
        TermTm (TermLam term) ->
            case term.out of
                TermAbs var body ->
                    ppParen (env.precedence >= 3)
                        (column
                            []
                            [ text (symbolToString var ++ " -> ")
                            , row
                                []
                                [ text "  "
                                , viewTerm2
                                    { model = env.model
                                    , precedence = 2
                                    , blockContext = Block
                                    , infixContext = NonInfix
                                    }
                                    body
                                ]
                            ]
                        )

                _ ->
                    impossible "viewTerm: TermLam, then no TermAbs?"

        TermTm (TermIf t1 t2 t3) ->
            text "(not implemented: TermIf)"

        TermTm (TermAnd t1 t2) ->
            text "(not implemented: TermAnd)"

        TermTm (TermOr t1 t2) ->
            text "(not implemented: TermOr)"

        TermTm (TermLetRec _ _ _) ->
            text "(not implemented: TermLetRec)"

        TermTm (TermLet _ _ _) ->
            text "(not implemented: TermLet)"

        TermTm (TermMatch _ _) ->
            text "(not implemented: TermMatch)"

        TermCycle _ ->
            text "(not implemented: TypeCycle)"


viewConstructor :
    Model
    -> Reference
    -> Int
    -> Element message
viewConstructor model reference n =
    let
        referent : Referent
        referent =
            Con reference n Data

        fallback : Element message
        fallback =
            viewReferent
                { showBuiltin = True
                , take = Just 7
                }
                referent
    in
    case model.codebase.head of
        Nothing ->
            fallback

        Just hash ->
            case HashDict.get hash model.codebase.branches of
                Nothing ->
                    fallback

                Just (Branch causal) ->
                    let
                        head =
                            rawCausalHead causal
                    in
                    case HashDict.get referent head.cache.termToName of
                        Nothing ->
                            fallback

                        Just names ->
                            case HashSet.toList names of
                                [] ->
                                    impossible "viewConstructor: empty names"

                                -- TODO, we should handle aliases better. this
                                -- just takes the first name
                                name :: _ ->
                                    viewConstructor2 head.cache.nameToTerm name


viewConstructor2 :
    NameDict ReferentSet
    -> Name
    -> Element message
viewConstructor2 nameToTerm fullName =
    text (String.join "." (shortenName nameToTerm fullName))
