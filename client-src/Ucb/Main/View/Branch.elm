module Ucb.Main.View.Branch exposing (viewBranch)

import Element exposing (..)
import Element.Events exposing (..)
import Element.Font exposing (..)
import HashingContainers.HashDict as HashDict
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Misc exposing (maybe)
import Ucb.Main.Message exposing (..)
import Ucb.Main.Model exposing (..)
import Ucb.Main.View.Reference exposing (viewId, viewReference)
import Ucb.Main.View.Referent exposing (viewReferent)
import Ucb.Main.View.Symbol exposing (viewSymbol)
import Ucb.Main.View.Term exposing (viewTerm)
import Ucb.Main.View.Type exposing (viewType)
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.NameSegment exposing (..)
import Unison.ConstructorType exposing (..)
import Unison.Declaration exposing (..)
import Unison.Reference exposing (..)
import Unison.Referent exposing (..)
import Unison.Symbol exposing (..)
import Unison.Util.Relation exposing (..)


viewBranch :
    Model
    -> BranchHash
    -> Branch
    -> Element Message
viewBranch model hash (Branch causal) =
    viewCausal model hash causal


viewBranch0 :
    Model
    -> Branch0
    -> Element Message
viewBranch0 model { terms, types, children, edits, cache } =
    column
        [ spacing 10 ]
        [ column
            [ spacing 5 ]
            (List.map
                (\( reference, name ) ->
                    viewBranchType
                        model
                        reference
                        name
                        (HashDict.get reference types.d3.domain)
                )
                (types.d1
                    |> relationToList
                    |> List.sortBy Tuple.second
                )
            )
        , column
            [ spacing 5 ]
            (List.map
                (\( referent, name ) ->
                    viewBranchTerm
                        model
                        referent
                        name
                        (HashDict.get referent terms.d3.domain)
                )
                (terms.d1
                    |> relationToList
                    |> List.sortBy Tuple.second
                )
            )
        , column
            [ spacing 5 ]
            (List.map
                (\( name, ( hash, branch ) ) ->
                    viewBranchChild model name hash branch
                )
                (children
                    |> HashDict.toList
                    |> List.sortBy Tuple.first
                )
            )

        -- , column [] (List.map text edits)
        ]


{-| View a child branch.
-}
viewBranchChild :
    Model
    -> NameSegment
    -> BranchHash
    -> Branch
    -> Element Message
viewBranchChild model name hash branch =
    column
        []
        [ el
            [ onClick (User_ToggleBranch hash)
            , pointer
            ]
            (row
                [ spacing 5 ]
                [ el [ bold ] (text "child")
                , text name
                ]
            )
        , case HashDict.get hash model.ui.branches of
            Just True ->
                el
                    [ paddingEach { bottom = 0, left = 10, right = 0, top = 0 } ]
                    (viewBranch model hash branch)

            _ ->
                none
        ]


{-| View a term in a branch.
-}
viewBranchTerm :
    Model
    -> Referent
    -> NameSegment
    -> Maybe (HashSet ( Reference, Reference ))
    -> Element Message
viewBranchTerm model referent name links =
    case referent of
        Ref reference ->
            viewBranchTerm2 model reference name links

        Con _ _ _ ->
            none


viewBranchTerm2 :
    Model
    -> Reference
    -> NameSegment
    -> Maybe (HashSet ( Reference, Reference ))
    -> Element Message
viewBranchTerm2 model reference name links =
    case reference of
        Builtin _ ->
            row
                [ spacing 5 ]
                [ el [ bold ] (text "builtin term")
                , text name
                ]

        Derived id ->
            column []
                [ row
                    [ onClick (User_ToggleTerm id)
                    , pointer
                    , spacing 5
                    ]
                    [ text name
                    , maybe
                        none
                        (\type_ ->
                            row [ spacing 5 ]
                                [ text ":"
                                , viewType model type_
                                ]
                        )
                        (HashDict.get id model.codebase.termTypes)
                    , viewId (Just 7) id
                    , viewLinks links
                    ]
                , case HashDict.get id model.ui.terms of
                    Just True ->
                        maybe
                            none
                            (\term ->
                                el
                                    [ paddingEach { bottom = 5, left = 10, right = 0, top = 5 } ]
                                    (viewTerm model term)
                            )
                            (HashDict.get id model.codebase.terms)

                    _ ->
                        none
                ]


{-| View a type in a branch.
-}
viewBranchType :
    Model
    -> Reference
    -> NameSegment
    -> Maybe (HashSet ( Reference, Reference ))
    -> Element message
viewBranchType model reference name links =
    case reference of
        Builtin _ ->
            row
                [ spacing 5 ]
                [ el [ bold ] (text "builtin type")
                , text name
                , viewLinks links
                ]

        Derived id ->
            case HashDict.get id model.codebase.types of
                Nothing ->
                    none

                Just declaration ->
                    case declaration of
                        DataDecl dataDeclaration ->
                            viewBranchType2 model name links id dataDeclaration Data

                        EffectDecl dataDeclaration ->
                            viewBranchType2 model name links id dataDeclaration Effect


viewBranchType2 :
    Model
    -> NameSegment
    -> Maybe (HashSet ( Reference, Reference ))
    -> Id
    -> DataDeclaration Symbol
    -> ConstructorType
    -> Element message
viewBranchType2 model name links id declaration constructorType =
    column
        []
        [ row
            [ spacing 5
            ]
            [ el [ bold ]
                (case constructorType of
                    Data ->
                        case declaration.modifier of
                            Structural ->
                                text "structural type"

                            Unique _ ->
                                text "unique type"

                    Effect ->
                        case declaration.modifier of
                            Structural ->
                                text "structural ability"

                            Unique _ ->
                                text "unique ability"
                )
            , text name
            , case declaration.bound of
                [] ->
                    none

                bound ->
                    row
                        [ spacing 5 ]
                        (List.map viewSymbol declaration.bound)
            , viewId (Just 7) id
            ]
        , el
            [ paddingEach { bottom = 5, left = 10, right = 0, top = 5 } ]
            (column
                []
                (List.map
                    (\( constructorName, type_ ) ->
                        row
                            [ spacing 5 ]
                            [ viewSymbol constructorName
                            , text ":"
                            , viewType model type_
                            ]
                    )
                    declaration.constructors
                )
            )
        ]


viewCausal :
    Model
    -> BranchHash
    -> RawCausal Branch0
    -> Element Message
viewCausal model hash causal =
    let
        viewHash :
            BranchHash
            -> Element Message
        viewHash hash_ =
            el
                [ onClick (User_FocusBranch hash_)
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

        viewPredecessors :
            List BranchHash
            -> Element Message
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
                    , viewBranch0 model branch
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
                    , viewBranch0 model branch
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
                    , viewBranch0 model branch
                    ]
        )


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
                        (Tuple.second
                            >> viewReference
                                { showBuiltin = True
                                , take = Just 7
                                }
                        )
                        (HashSet.toList links)
                )
