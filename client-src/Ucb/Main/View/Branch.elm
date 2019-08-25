module Ucb.Main.View.Branch exposing (viewBranch)

import Element exposing (..)
import Element.Events exposing (..)
import Element.Font exposing (..)
import HashingContainers.HashDict as HashDict
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Misc exposing (maybe)
import Ucb.Main.Message exposing (..)
import Ucb.Main.Model exposing (..)
import Ucb.Main.View.Declaration exposing (viewDeclaration)
import Ucb.Main.View.Reference exposing (viewId, viewReference)
import Ucb.Main.View.Referent exposing (viewReferent)
import Ucb.Main.View.Term exposing (viewTerm)
import Ucb.Main.View.Type exposing (viewType)
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.NameSegment exposing (..)
import Unison.ConstructorType exposing (..)
import Unison.Reference exposing (..)
import Unison.Referent exposing (..)
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
viewBranch0 model { terms, types, children, edits } =
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
                        (HashDict.get ref types.d3.domain)
                )
                (types.d1
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
                        (HashDict.get ref terms.d3.domain)
                )
                (terms.d1
                    |> relationToList
                    |> List.sortBy Tuple.second
                )
            )
        , column
            []
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

                    Con _ _ Data ->
                        text "constructor"

                    Con _ _ Effect ->
                        text "request"
                )
            , text nameSegment
            , viewReferent
                { showBuiltin = False
                , take = Just 7
                }
                referent
            , viewLinks links
            ]
        , maybe
            none
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
                                [ maybe
                                    none
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
            column
                []
                [ row
                    [ onClick (User_GetType id)
                    , pointer
                    , spacing 5
                    ]
                    [ el [ bold ] (text "type")
                    , text name
                    , viewId (Just 7) id
                    , viewLinks links
                    ]
                , maybe
                    none
                    (\declaration ->
                        el
                            [ paddingEach
                                { bottom = 5
                                , left = 10
                                , right = 0
                                , top = 5
                                }
                            ]
                            (viewDeclaration declaration)
                    )
                    (HashDict.get id model.codebase.types)
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
