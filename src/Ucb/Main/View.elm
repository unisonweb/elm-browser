module Ucb.Main.View exposing (..)

import Bytes
import Element exposing (..)
import Element.Events exposing (..)
import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Html exposing (Html)
import Misc exposing (hashSetSize)
import Ucb.Main.Message exposing (..)
import Ucb.Main.Model exposing (Error(..), Model)
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.NameSegment exposing (..)
import Unison.Hash exposing (..)
import Unison.Util.Relation exposing (..)


view : Model -> Html Message
view model =
    layout
        [ padding 10 ]
        (view2 model)


view2 : Model -> Element Message
view2 model =
    column
        [ spacing 10 ]
        (List.filterMap identity
            [ Maybe.map text model.head
            , model.head
                |> Maybe.andThen
                    (\head ->
                        Maybe.map
                            (viewRawCausal model.codebase.branches)
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


viewError :
    Error
    -> Element message
viewError error =
    text (Debug.toString error)


viewRawBranch :
    HashDict Hash32 RawCausal
    -> RawBranch
    -> Element Message
viewRawBranch branches branch =
    let
        terms : List NameSegment
        terms =
            relationRange branch.terms.d1

        types : List NameSegment
        types =
            relationRange branch.types.d1

        children : List ( NameSegment, Hash32 )
        children =
            HashDict.toList branch.children

        edits : List NameSegment
        edits =
            List.map Tuple.first (HashDict.toList branch.edits)
    in
    column []
        [ column [] (List.map text terms)
        , column [] (List.map text types)
        , column
            []
            (List.map
                (\( name, hash ) ->
                    el
                        [ onClick (User_GetBranch { hash = hash, focus = False }) ]
                        (column
                            []
                            [ text name
                            , el
                                [ paddingEach
                                    { bottom = 0
                                    , left = 10
                                    , right = 0
                                    , top = 0
                                    }
                                ]
                                (viewMaybe
                                    (viewRawCausal branches)
                                    (HashDict.get hash branches)
                                )
                            ]
                        )
                )
                children
            )
        , column [] (List.map text edits)
        ]


viewRawCausal :
    HashDict Hash32 RawCausal
    -> RawCausal
    -> Element Message
viewRawCausal branches causal =
    let
        viewPrevBranch : Hash32 -> Element Message
        viewPrevBranch hash =
            el
                [ onClick (User_GetBranch { hash = hash, focus = True }) ]
                (text hash)
    in
    case causal of
        RawOne branch ->
            viewRawBranch branches branch

        RawCons branch hash ->
            column
                [ spacing 10 ]
                [ row
                    [ spacing 10 ]
                    [ text "Prev"
                    , viewPrevBranch hash
                    ]
                , viewRawBranch
                    branches
                    branch
                ]

        RawMerge branch hashes ->
            column
                [ spacing 10 ]
                [ row
                    [ spacing 10 ]
                    [ text "Prev"
                    , row [] (List.map viewPrevBranch (HashSet.toList hashes))
                    ]
                , viewRawBranch
                    branches
                    branch
                ]


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
