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
import Unison.Hash exposing (..)
import Unison.Reference exposing (..)
import Unison.Referent exposing (..)
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
            [ model.head
                |> Maybe.andThen
                    (\head ->
                        Maybe.map
                            (viewRawCausal
                                model.codebase.branches
                                model.codebase.successors
                                model.ui.branches
                                head
                            )
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
    HashDict Hash32 RawCausal
    -> HashDict Hash32 (HashSet Hash32)
    -> HashDict Hash32 Bool
    -> NameSegment
    -> Hash32
    -> Element Message
viewBranchChild branches successors visible name hash =
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
                    case HashDict.get hash visible of
                        Just True ->
                            viewRawCausal
                                branches
                                successors
                                visible
                                hash
                                causal

                        _ ->
                            none
                )
                (HashDict.get hash branches)
            )
        ]


{-| View a term in a branch.
-}
viewBranchTerm :
    Referent
    -> NameSegment
    -> Element Message
viewBranchTerm referent nameSegment =
    row
        [ spacing 5 ]
        [ el [ bold ] (text "term")
        , text nameSegment
        , viewShortReferent referent
        ]


{-| View a type in a branch.
-}
viewBranchType :
    Reference
    -> NameSegment
    -> Element Message
viewBranchType reference nameSegment =
    row
        [ spacing 5 ]
        [ el [ bold ] (text "type")
        , text nameSegment
        , viewShortReference reference
        ]


viewError :
    Error
    -> Element message
viewError error =
    text (Debug.toString error)


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
        Builtin string ->
            text ("Builtin#" ++ string)

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
    HashDict Hash32 RawCausal
    -> HashDict Hash32 (HashSet Hash32)
    -> HashDict Hash32 Bool
    -> RawBranch
    -> Element Message
viewRawBranch branches successors visible branch =
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
                -- TODO metadata
                (\( ref, name ) -> viewBranchType ref name)
                (branch.types.d1
                    |> relationToList
                    |> List.sortBy Tuple.second
                )
            )
        , column
            []
            (List.map
                (\( ref, name ) -> viewBranchTerm ref name)
                -- TODO metadata
                (branch.terms.d1
                    |> relationToList
                    |> List.sortBy Tuple.second
                )
            )
        , column
            []
            (List.map
                (\( name, hash ) ->
                    viewBranchChild branches successors visible name hash
                )
                (branch.children
                    |> HashDict.toList
                    |> List.sortBy Tuple.first
                )
            )
        , column [] (List.map text edits)
        ]


viewRawCausal :
    HashDict Hash32 RawCausal
    -> HashDict Hash32 (HashSet Hash32)
    -> HashDict Hash32 Bool
    -> Hash32
    -> RawCausal
    -> Element Message
viewRawCausal branches successors visible hash causal =
    let
        viewHash : Hash32 -> Element Message
        viewHash hash_ =
            el
                [ onClick (User_GetBranch { hash = hash_, focus = True })
                , pointer
                ]
                (text hash_)

        viewPredecessors : List Hash32 -> Element Message
        viewPredecessors hashes =
            row
                [ spacing 10 ]
                (text "Predecessors" :: List.map viewHash hashes)

        viewSuccessors : Element Message
        viewSuccessors =
            case HashDict.get hash successors of
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
                        , viewSuccessors
                        ]
                    , viewRawBranch branches successors visible branch
                    ]

            RawCons branch hash_ ->
                column
                    [ spacing 5 ]
                    [ column []
                        [ viewHash hash
                        , viewPredecessors [ hash_ ]
                        , viewSuccessors
                        ]
                    , viewRawBranch branches successors visible branch
                    ]

            RawMerge branch hashes ->
                column
                    [ spacing 5 ]
                    [ column []
                        [ viewHash hash
                        , viewPredecessors (HashSet.toList hashes)
                        , viewSuccessors
                        ]
                    , viewRawBranch branches successors visible branch
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
