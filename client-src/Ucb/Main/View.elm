module Ucb.Main.View exposing (..)

import Bytes
import Element exposing (..)
import Element.Events exposing (..)
import Element.Font exposing (..)
import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Html exposing (Html)
import Misc exposing (hashSetSize)
import Ucb.Main.Message exposing (..)
import Ucb.Main.Model exposing (..)
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.NameSegment exposing (..)
import Unison.ConstructorType exposing (..)
import Unison.Declaration exposing (..)
import Unison.Hash exposing (..)
import Unison.Reference exposing (..)
import Unison.Referent exposing (..)
import Unison.Symbol exposing (..)
import Unison.Term exposing (..)
import Unison.Type exposing (..)
import Unison.Util.Relation exposing (..)
import Unison.Var exposing (..)
import Word64 exposing (..)


view : Model -> Html Message
view model =
    layout
        []
        (view2 model)


view2 : Model -> Element Message
view2 model =
    column
        []
        (List.filterMap identity
            [ model.codebase.head
                |> Maybe.andThen
                    (\head ->
                        Maybe.map
                            (viewRawCausal model head)
                            (HashDict.get head model.codebase.branches)
                    )
            , if List.isEmpty model.errors then
                Nothing

              else
                Just
                    (column []
                        (text "Errors:"
                            :: List.map viewError (List.reverse model.errors)
                        )
                    )
            , Maybe.map
                (\limit -> text ("GitHub rate limit: " ++ limit))
                model.rateLimit
            ]
        )


{-| View a child branch.
-}
viewBranchChild :
    Model
    -> NameSegment
    -> BranchHash
    -> Element Message
viewBranchChild model name hash =
    column
        []
        [ el
            [ onClick (User_GetBranch { hash = hash, focus = False })
            , pointer
            ]
            (row
                [ spacing 5 ]
                [ el [ bold ] (text "child")
                , text name
                , viewShortHash hash
                ]
            )
        , el
            [ paddingEach
                { bottom = 0
                , left = 10
                , right = 0
                , top = 0
                }
            ]
            (viewMaybe
                (\causal ->
                    case HashDict.get hash model.ui.branches of
                        Just True ->
                            viewRawCausal model hash causal

                        _ ->
                            none
                )
                (HashDict.get hash model.codebase.branches)
            )
        ]


{-| View a term in a branch.
-}
viewBranchTerm :
    Model
    -> Referent
    -> NameSegment
    -> Maybe (HashSet ( Reference, Reference ))
    -> Element Message
viewBranchTerm model referent nameSegment links =
    column
        []
        [ row
            [ onClick (User_GetTerm referent)
            , pointer
            , spacing 5
            ]
            [ el [ bold ]
                (case referent of
                    Ref _ ->
                        text "term"

                    Con _ _ _ ->
                        text "constructor"
                )
            , text nameSegment
            , viewShortReferent False referent
            , viewLinks links
            ]
        , viewMaybe
            (\term ->
                case HashDict.get referent model.ui.terms of
                    Just True ->
                        el
                            [ paddingEach
                                { bottom = 5
                                , left = 10
                                , right = 0
                                , top = 5
                                }
                            ]
                            (column
                                []
                                [ viewMaybe
                                    viewType
                                    (HashDict.get referent model.codebase.termTypes)
                                , viewTerm term
                                ]
                            )

                    _ ->
                        none
            )
            (HashDict.get referent model.codebase.terms)
        ]


{-| View a type in a branch.
-}
viewBranchType :
    Model
    -> Reference
    -> NameSegment
    -> Maybe (HashSet ( Reference, Reference ))
    -> Element Message
viewBranchType model reference nameSegment links =
    column
        []
        [ row
            [ onClick (User_GetType reference)
            , pointer
            , spacing 5
            ]
            [ el [ bold ] (text "type")
            , text nameSegment
            , viewShortReference False reference
            , viewLinks links
            ]
        , viewMaybe
            (\declaration ->
                case HashDict.get reference model.ui.types of
                    Just True ->
                        el
                            [ paddingEach
                                { bottom = 5
                                , left = 10
                                , right = 0
                                , top = 5
                                }
                            ]
                            (viewDeclaration declaration)

                    _ ->
                        none
            )
            (HashDict.get reference model.codebase.types)
        ]


viewDataDeclaration :
    ConstructorType
    -> DataDeclaration Symbol
    -> Element message
viewDataDeclaration constructorType declaration =
    column
        []
        [ case constructorType of
            Data ->
                case declaration.modifier of
                    Structural ->
                        text "structural type"

                    Unique _ ->
                        text "unique type"

            Effect ->
                text "ability"
        , case declaration.bound of
            [] ->
                none

            bound ->
                row
                    [ spacing 5 ]
                    (text "bound" :: List.map viewSymbol declaration.bound)
        , case declaration.constructors of
            [] ->
                none

            constructors ->
                row
                    [ spacing 5 ]
                    (text "constructors"
                        :: List.map
                            (\( name, type_ ) ->
                                row [ spacing 2 ] [ viewSymbol name ]
                            )
                            declaration.constructors
                    )
        ]


viewDeclaration :
    Declaration Symbol
    -> Element message
viewDeclaration declaration =
    case declaration of
        DataDecl dataDeclaration ->
            viewDataDeclaration Data dataDeclaration

        EffectDecl dataDeclaration ->
            viewDataDeclaration Effect dataDeclaration


viewError :
    Error
    -> Element message
viewError error =
    text (Debug.toString error)


viewLinks :
    Maybe (HashSet ( Reference, Reference ))
    -> Element message
viewLinks maybeLinks =
    case maybeLinks of
        Nothing ->
            none

        Just links ->
            row [ spacing 5 ]
                (text "links"
                    :: List.map
                        (Tuple.second >> viewShortReference True)
                        (HashSet.toList links)
                )


viewShortId : Id -> Element message
viewShortId { hash, pos, size } =
    row
        []
        [ viewShortHash hash
        , if size > 1 then
            el
                [ color (rgb 0.5 0.5 0.5) ]
                (text (String.cons '#' (String.fromInt pos)))

          else
            none
        ]


viewShortReference :
    Bool
    -> Reference
    -> Element message
viewShortReference showBuiltin reference =
    case reference of
        Builtin name ->
            if showBuiltin then
                text name

            else
                none

        Derived id ->
            viewShortId id


viewShortReferent :
    Bool
    -> Referent
    -> Element message
viewShortReferent showBuiltin referent =
    case referent of
        Ref reference ->
            viewShortReference showBuiltin reference

        Con reference _ _ ->
            viewShortReference showBuiltin reference


{-| View a raw branch.
-}
viewRawBranch :
    Model
    -> RawBranch
    -> Element Message
viewRawBranch model branch =
    let
        edits : List NameSegment
        edits =
            List.map Tuple.first (HashDict.toList branch.edits)
    in
    column
        [ spacing 5 ]
        [ column
            []
            (List.map
                (\( ref, name ) ->
                    viewBranchType
                        model
                        ref
                        name
                        (HashDict.get ref branch.types.d3.domain)
                )
                (branch.types.d1
                    |> relationToList
                    |> List.sortBy Tuple.second
                )
            )
        , column
            []
            (List.map
                (\( ref, name ) ->
                    viewBranchTerm
                        model
                        ref
                        name
                        (HashDict.get ref branch.terms.d3.domain)
                )
                (branch.terms.d1
                    |> relationToList
                    |> List.sortBy Tuple.second
                )
            )
        , column
            []
            (List.map
                (\( name, hash ) -> viewBranchChild model name hash)
                (branch.children
                    |> HashDict.toList
                    |> List.sortBy Tuple.first
                )
            )
        , column [] (List.map text edits)
        ]


viewRawCausal :
    Model
    -> BranchHash
    -> RawCausal RawBranch
    -> Element Message
viewRawCausal model hash causal =
    let
        viewHash : BranchHash -> Element Message
        viewHash hash_ =
            el
                [ onClick (User_GetBranch { hash = hash_, focus = True })
                , pointer
                ]
                (text hash_)

        viewParents : Element Message
        viewParents =
            case HashDict.get hash model.codebase.parents of
                Nothing ->
                    none

                Just hashes ->
                    row
                        [ spacing 10 ]
                        (text "Parents" :: List.map viewHash (HashSet.toList hashes))

        viewPredecessors : List BranchHash -> Element Message
        viewPredecessors hashes =
            row
                [ spacing 10 ]
                (text "Predecessors" :: List.map viewHash hashes)

        viewSuccessors : Element Message
        viewSuccessors =
            case HashDict.get hash model.codebase.successors of
                Nothing ->
                    none

                Just hashes ->
                    row
                        [ spacing 10 ]
                        (text "Successors" :: List.map viewHash (HashSet.toList hashes))
    in
    el [ padding 10 ]
        (case causal of
            RawOne branch ->
                column
                    [ spacing 5 ]
                    [ column []
                        [ viewHash hash
                        , viewParents
                        , viewSuccessors
                        ]
                    , viewRawBranch model branch
                    ]

            RawCons branch hash_ ->
                column
                    [ spacing 5 ]
                    [ column []
                        [ viewHash hash
                        , viewParents
                        , viewPredecessors [ hash_ ]
                        , viewSuccessors
                        ]
                    , viewRawBranch model branch
                    ]

            RawMerge branch hashes ->
                column
                    [ spacing 5 ]
                    [ column []
                        [ viewHash hash
                        , viewParents
                        , viewPredecessors (HashSet.toList hashes)
                        , viewSuccessors
                        ]
                    , viewRawBranch model branch
                    ]
        )


viewShortHash :
    Hash32
    -> Element message
viewShortHash hash =
    el [ color (rgb 0.5 0.5 0.5) ] (text (String.left 7 hash))


viewSymbol :
    Symbol
    -> Element message
viewSymbol symbol =
    case symbol of
        Symbol n var ->
            case var of
                User name ->
                    let
                        m : Int
                        m =
                            unsafeWord64ToWord53 n
                    in
                    text
                        (name
                            ++ (if m == 0 then
                                    ""

                                else
                                    String.fromInt m
                               )
                        )

                _ ->
                    none


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


viewType :
    Type Symbol
    -> Element message
viewType { out } =
    case out of
        TypeVar var ->
            viewSymbol var

        TypeAbs var ty ->
            row
                [ spacing 2 ]
                [ viewSymbol var
                , text "."
                , viewType ty
                ]

        TypeTm (TypeRef ref) ->
            viewShortReference True ref

        TypeTm (TypeArrow ty1 ty2) ->
            row
                [ spacing 2 ]
                [ text "("
                , viewType ty1
                , text "->"
                , viewType ty2
                , text ")"
                ]

        TypeTm (TypeApp ty1 ty2) ->
            row
                [ spacing 2 ]
                [ text "("
                , viewType ty1
                , viewType ty2
                , text ")"
                ]

        TypeTm (TypeEffect ty1 ty2) ->
            row
                [ spacing 2 ]
                [ text "("
                , viewType ty1
                , viewType ty2
                , text ")"
                ]

        TypeTm (TypeEffects tys) ->
            row
                [ spacing 2 ]
                [ text "{"
                , row [] (List.map viewType tys)
                , text "}"
                ]

        TypeTm (TypeForall ty) ->
            row
                [ spacing 2 ]
                [ text "∀"
                , viewType ty
                ]

        TypeTm (TypeIntroOuter ty) ->
            viewType ty

        TypeCycle _ ->
            text "TypeCycle"

        TypeTm (TypeAnn _ _) ->
            text "TypeAnn"


viewMaybe :
    (a -> Element message)
    -> Maybe a
    -> Element message
viewMaybe f mx =
    case mx of
        Nothing ->
            none

        Just x ->
            f x
