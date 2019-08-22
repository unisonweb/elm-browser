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
import Unison.Declaration exposing (..)
import Unison.Hash exposing (..)
import Unison.Reference exposing (..)
import Unison.Referent exposing (..)
import Unison.Symbol exposing (..)
import Unison.Type exposing (..)
import Unison.Util.Relation exposing (..)


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
    -> Hash32
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
    Referent
    -> NameSegment
    -> Maybe (HashSet ( Reference, Reference ))
    -> Element Message
viewBranchTerm referent nameSegment links =
    row
        [ spacing 5 ]
        [ el [ bold ] (text "term")
        , text nameSegment
        , viewShortReferent referent
        , viewLinks links
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
            , viewShortReference reference
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


viewDeclaration :
    Declaration Symbol
    -> Element message
viewDeclaration =
    Debug.toString >> text


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
                        (Tuple.second >> viewShortReference)
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
    Reference
    -> Element message
viewShortReference reference =
    case reference of
        Builtin _ ->
            none

        Derived id ->
            viewShortId id


viewShortReferent :
    Referent
    -> Element message
viewShortReferent referent =
    case referent of
        Ref reference ->
            viewShortReference reference

        Con reference _ _ ->
            viewShortReference reference


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
    -> Hash32
    -> RawCausal
    -> Element Message
viewRawCausal model hash causal =
    let
        viewHash : Hash32 -> Element Message
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

        viewPredecessors : List Hash32 -> Element Message
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
