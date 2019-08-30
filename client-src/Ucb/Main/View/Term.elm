module Ucb.Main.View.Term exposing (viewTerm)

import Array exposing (Array)
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
import Unison.Type exposing (..)
import Word64 exposing (..)


type alias Env =
    { model : Model -- TODO pick out the bits we actually care about
    , precedence : Int
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
    Env
    -> Term Symbol
    -> Element message
viewTerm2 env { out } =
    case out of
        TermAbs var term ->
            text "(not implemented: TermAbs)"

        TermCycle _ ->
            text "(not implemented: TermCycle)"

        TermVar var ->
            viewTermVar var

        TermTm (TermApp t1 t2) ->
            viewTermApp env t1 t2

        TermTm (TermAnd t1 t2) ->
            viewTermAnd env t1 t2

        TermTm (TermAnn term type_) ->
            viewTermAnn env term type_

        TermTm (TermBlank blank) ->
            text "(not implemented: TermBlank)"

        TermTm (TermBoolean b) ->
            viewTermBoolean b

        TermTm (TermChar c) ->
            viewTermChar c

        TermTm (TermConstructor reference n) ->
            viewTermConstructor env reference n

        TermTm (TermFloat n) ->
            viewTermFloat n

        TermTm (TermHandle t1 t2) ->
            viewTermHandle env t1 t2

        TermTm (TermIf t1 t2 t3) ->
            viewTermIf env t1 t2 t3

        TermTm (TermInt n) ->
            viewTermInt n

        TermTm (TermLam term) ->
            viewTermLam env term

        TermTm (TermLet _ _ _) ->
            text "(not implemented: TermLet)"

        TermTm (TermLetRec _ _ _) ->
            text "(not implemented: TermLetRec)"

        TermTm (TermMatch _ _) ->
            text "(not implemented: TermMatch)"

        TermTm (TermNat n) ->
            viewTermNat n

        TermTm (TermOr t1 t2) ->
            viewTermOr env t1 t2

        TermTm (TermRef reference) ->
            viewTermRef env reference

        TermTm (TermRequest reference n) ->
            viewTermRequest env reference n

        TermTm (TermSequence terms) ->
            viewTermSequence env terms

        TermTm (TermText s) ->
            text "(not implemented: TermText)"


{-| Should be the same as viewTermOr
-}
viewTermAnd :
    Env
    -> Term Symbol
    -> Term Symbol
    -> Element message
viewTermAnd env t1 t2 =
    let
        env2 : Env
        env2 =
            { model = env.model
            , precedence = 10
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
                    [ viewTerm2 env2 t1
                    , viewTerm2 env2 t2
                    ]
                ]
            ]
        )


viewTermAnn :
    Env
    -> Term Symbol
    -> Type Symbol
    -> Element message
viewTermAnn env term type_ =
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


viewTermApp :
    Env
    -> Term Symbol
    -> Term Symbol
    -> Element message
viewTermApp env t1 t2 =
    case termUnApps t1 t2 of
        ( f, xs ) ->
            ppParen
                (env.precedence >= 10)
                (row []
                    ((f :: xs)
                        |> List.map
                            (viewTerm2
                                { model = env.model
                                , precedence = 10
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
    Env
    -> Reference
    -> Int
    -> Element message
viewTermConstructor env reference n =
    viewReferent_ env.model (Con reference n Data)


viewTermFloat :
    Float
    -> Element message
viewTermFloat n =
    text (String.fromFloat n)


viewTermHandle :
    Env
    -> Term Symbol
    -> Term Symbol
    -> Element message
viewTermHandle env t1 t2 =
    let
        env2 : Env
        env2 =
            { model = env.model
            , precedence = 10
            , blockContext = Normal
            , infixContext = NonInfix
            }
    in
    ppParen (env.precedence >= 2)
        (column []
            [ row []
                [ text "handle "
                , viewTerm2
                    { model = env.model
                    , precedence = 2
                    , blockContext = Normal
                    , infixContext = NonInfix
                    }
                    t1
                , text "in"
                ]
            , row []
                [ text "  "
                , viewTerm2
                    { model = env.model
                    , precedence = 2
                    , blockContext = Block
                    , infixContext = NonInfix
                    }
                    t2
                ]
            ]
        )


viewTermIf :
    Env
    -> Term Symbol
    -> Term Symbol
    -> Term Symbol
    -> Element message
viewTermIf env t1 t2 t3 =
    ppParen (env.precedence >= 2)
        (column []
            [ row []
                [ text "if "
                , viewTerm2
                    { model = env.model
                    , precedence = 2
                    , blockContext = Block
                    , infixContext = NonInfix
                    }
                    t1
                , text " then"
                ]
            , row []
                [ text "  "
                , viewTerm2
                    { model = env.model
                    , precedence = 0
                    , blockContext = Block
                    , infixContext = NonInfix
                    }
                    t2
                ]
            , text "else"
            , row []
                [ text "  "
                , viewTerm2
                    { model = env.model
                    , precedence = 0
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
    Env
    -> Term Symbol
    -> Element message
viewTermLam env term =
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
                            { model = env.model
                            , precedence = 2
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
    Env
    -> Term Symbol
    -> Term Symbol
    -> Element message
viewTermOr env t1 t2 =
    let
        env2 : Env
        env2 =
            { model = env.model
            , precedence = 10
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
                    [ viewTerm2 env2 t1
                    , viewTerm2 env2 t2
                    ]
                ]
            ]
        )


viewTermRef :
    Env
    -> Reference
    -> Element message
viewTermRef env reference =
    viewReferent_ env.model (Ref reference)


viewTermRequest :
    Env
    -> Reference
    -> Int
    -> Element message
viewTermRequest env reference n =
    viewReferent_ env.model (Con reference n Effect)


viewTermSequence :
    Env
    -> Array (Term Symbol)
    -> Element message
viewTermSequence env terms =
    row []
        [ text "["
        , row []
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


viewTermVar :
    Symbol
    -> Element message
viewTermVar var =
    text (symbolToString var)


viewReferent_ :
    Model
    -> Referent
    -> Element message
viewReferent_ model referent =
    let
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
