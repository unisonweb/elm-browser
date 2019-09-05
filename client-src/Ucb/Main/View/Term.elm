module Ucb.Main.View.Term exposing (viewTerm)

import Array exposing (Array)
import Element exposing (..)
import HashingContainers.HashDict as HashDict
import HashingContainers.HashSet as HashSet
import Int64 exposing (..)
import Misc exposing (..)
import Ucb.Main.View.Referent exposing (viewReferent)
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
import Unison.Type exposing (..)
import Word64 exposing (..)


type alias Env =
    { precedence : Int
    , blockContext : BlockContext
    , infixContext : InfixContext
    }


type BlockContext
    = Block
    | Normal


type InfixContext
    = Infix
    | NonInfix


viewTerm :
    { r | head : Branch }
    -> Term Symbol
    -> Element message
viewTerm view =
    viewTerm2
        view
        { precedence = -1
        , blockContext = Normal
        , infixContext = NonInfix
        }


viewTerm2 :
    { r | head : Branch }
    -> Env
    -> Term Symbol
    -> Element message
viewTerm2 view env { out } =
    case out of
        TermAbs var term ->
            text "(not implemented: TermAbs)"

        TermCycle _ ->
            text "(not implemented: TermCycle)"

        TermVar var ->
            viewTermVar var

        TermTm (TermApp t1 t2) ->
            viewTermApp view env t1 t2

        TermTm (TermAnd t1 t2) ->
            viewTermAnd view env t1 t2

        TermTm (TermAnn term type_) ->
            viewTermAnn view env term type_

        TermTm (TermBlank blank) ->
            text "(not implemented: TermBlank)"

        TermTm (TermBoolean b) ->
            viewTermBoolean b

        TermTm (TermChar c) ->
            viewTermChar c

        TermTm (TermConstructor reference n) ->
            viewTermConstructor view reference n

        TermTm (TermFloat n) ->
            viewTermFloat n

        TermTm (TermHandle t1 t2) ->
            viewTermHandle view env t1 t2

        TermTm (TermIf t1 t2 t3) ->
            viewTermIf view env t1 t2 t3

        TermTm (TermInt n) ->
            viewTermInt n

        TermTm (TermLam term) ->
            viewTermLam view env term

        TermTm (TermLet _ _ _) ->
            text "(not implemented: TermLet)"

        TermTm (TermLetRec _ _ _) ->
            text "(not implemented: TermLetRec)"

        TermTm (TermMatch _ _) ->
            text "(not implemented: TermMatch)"

        TermTm (TermNat n) ->
            viewTermNat n

        TermTm (TermOr t1 t2) ->
            viewTermOr view env t1 t2

        TermTm (TermRef reference) ->
            viewTermRef view reference

        TermTm (TermRequest reference n) ->
            viewTermRequest view reference n

        TermTm (TermSequence terms) ->
            viewTermSequence view env terms

        TermTm (TermText s) ->
            text "(not implemented: TermText)"


{-| Should be the same as viewTermOr
-}
viewTermAnd :
    { r | head : Branch }
    -> Env
    -> Term Symbol
    -> Term Symbol
    -> Element message
viewTermAnd view env t1 t2 =
    let
        env2 : Env
        env2 =
            { precedence = 10
            , blockContext = Normal
            , infixContext = NonInfix
            }
    in
    ppParen (env.precedence >= 10)
        (column []
            [ text "and"
            , row []
                [ text "  "
                , column []
                    [ viewTerm2 view env2 t1
                    , viewTerm2 view env2 t2
                    ]
                ]
            ]
        )


viewTermAnn :
    { r | head : Branch }
    -> Env
    -> Term Symbol
    -> Type Symbol
    -> Element message
viewTermAnn view env term _ =
    viewTerm2 view env term


viewTermApp :
    { r | head : Branch }
    -> Env
    -> Term Symbol
    -> Term Symbol
    -> Element message
viewTermApp view env t1 t2 =
    case termUnApps t1 t2 of
        ( f, xs ) ->
            ppParen
                (env.precedence >= 10)
                (row []
                    ((f :: xs)
                        |> List.map
                            (viewTerm2
                                view
                                { precedence = 10
                                , blockContext = Normal
                                , infixContext = NonInfix
                                }
                            )
                        |> List.intersperse (text " ")
                    )
                )


viewTermBoolean :
    Bool
    -> Element message
viewTermBoolean b =
    text
        (if b then
            "true"

         else
            "false"
        )


viewTermChar :
    Char
    -> Element message
viewTermChar c =
    text ("'" ++ String.fromChar c ++ "'")


viewTermConstructor :
    { r | head : Branch }
    -> Reference
    -> Int
    -> Element message
viewTermConstructor view reference n =
    viewReferent_ view (Con reference n Data)


viewTermFloat :
    Float
    -> Element message
viewTermFloat n =
    text (String.fromFloat n)


viewTermHandle :
    { r | head : Branch }
    -> Env
    -> Term Symbol
    -> Term Symbol
    -> Element message
viewTermHandle view env t1 t2 =
    ppParen (env.precedence >= 2)
        (column []
            [ row []
                [ text "handle "
                , viewTerm2
                    view
                    { precedence = 2
                    , blockContext = Normal
                    , infixContext = NonInfix
                    }
                    t1
                , text "in"
                ]
            , row []
                [ text "  "
                , viewTerm2
                    view
                    { precedence = 2
                    , blockContext = Block
                    , infixContext = NonInfix
                    }
                    t2
                ]
            ]
        )


viewTermIf :
    { r | head : Branch }
    -> Env
    -> Term Symbol
    -> Term Symbol
    -> Term Symbol
    -> Element message
viewTermIf view env t1 t2 t3 =
    ppParen (env.precedence >= 2)
        (column []
            [ row []
                [ text "if "
                , viewTerm2
                    view
                    { precedence = 2
                    , blockContext = Block
                    , infixContext = NonInfix
                    }
                    t1
                , text " then"
                ]
            , row []
                [ text "  "
                , viewTerm2
                    view
                    { precedence = 0
                    , blockContext = Block
                    , infixContext = NonInfix
                    }
                    t2
                ]
            , text "else"
            , row []
                [ text "  "
                , viewTerm2
                    view
                    { precedence = 0
                    , blockContext = Block
                    , infixContext = NonInfix
                    }
                    t3
                ]
            ]
        )


viewTermInt :
    Int64
    -> Element message
viewTermInt n =
    let
        n2 : Int
        n2 =
            unsafeInt64ToInt53 n
    in
    if n2 >= 0 then
        text (String.cons '+' (String.fromInt n2))

    else
        text (String.fromInt n2)


viewTermLam :
    { r | head : Branch }
    -> Env
    -> Term Symbol
    -> Element message
viewTermLam view env term =
    case termUnLams term of
        ( vars, body ) ->
            ppParen (env.precedence >= 3)
                (column []
                    [ row []
                        [ vars
                            |> List.map (\var -> text (symbolToString var ++ " "))
                            |> row []
                        , text "->"
                        ]
                    , row []
                        [ text "  "
                        , viewTerm2
                            view
                            { precedence = 2
                            , blockContext = Block
                            , infixContext = NonInfix
                            }
                            body
                        ]
                    ]
                )


viewTermNat :
    Word64
    -> Element message
viewTermNat n =
    text (String.fromInt (unsafeWord64ToInt53 n))


{-| Should be the same as viewTermAnd
-}
viewTermOr :
    { r | head : Branch }
    -> Env
    -> Term Symbol
    -> Term Symbol
    -> Element message
viewTermOr view env t1 t2 =
    let
        env2 : Env
        env2 =
            { precedence = 10
            , blockContext = Normal
            , infixContext = NonInfix
            }
    in
    ppParen (env.precedence >= 10)
        (column []
            [ text "or"
            , row []
                [ text "  "
                , column []
                    [ viewTerm2 view env2 t1
                    , viewTerm2 view env2 t2
                    ]
                ]
            ]
        )


viewTermRef :
    { r | head : Branch }
    -> Reference
    -> Element message
viewTermRef view reference =
    viewReferent_ view (Ref reference)


viewTermRequest :
    { r | head : Branch }
    -> Reference
    -> Int
    -> Element message
viewTermRequest view reference n =
    viewReferent_ view (Con reference n Effect)


viewTermSequence :
    { r | head : Branch }
    -> Env
    -> Array (Term Symbol)
    -> Element message
viewTermSequence view env terms =
    row []
        [ text "["
        , row []
            (terms
                |> Array.map
                    (viewTerm2
                        view
                        { precedence = 0
                        , blockContext = Normal
                        , infixContext = NonInfix
                        }
                    )
                |> Array.toList
                |> List.intersperse (text ", ")
            )
        , text "]"
        ]


viewTermVar :
    Symbol
    -> Element message
viewTermVar var =
    text (symbolToString var)


viewReferent_ :
    { r | head : Branch }
    -> Referent
    -> Element message
viewReferent_ view referent =
    let
        fallback : Element message
        fallback =
            viewReferent
                { showBuiltin = True
                , take = Just 7
                }
                referent

        head : Branch0
        head =
            branchHead view.head
    in
    case HashDict.get referent head.cache.termToName of
        Nothing ->
            fallback

        Just names ->
            case HashSet.toList names of
                [] ->
                    impossible "viewReferent: empty names"

                -- TODO, we should handle aliases better. this
                -- just takes the first name
                name :: _ ->
                    viewReferent2 head.cache.nameToTerm name


viewReferent2 :
    NameDict ReferentSet
    -> Name
    -> Element message
viewReferent2 nameToTerm fullName =
    text (nameToString (shortenName nameToTerm fullName))
