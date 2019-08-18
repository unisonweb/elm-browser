module Ucb.Main.View exposing (..)

import Bytes
import Element exposing (..)
import Element.Events exposing (..)
import HashingContainers.HashDict as HashDict exposing (HashDict)
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
        [ text "Terms"
        , column [] (List.map text terms)
        , text "Types"
        , column [] (List.map text types)
        , text "Children"
        , column
            []
            (List.map
                (\( name, hash ) ->
                    el
                        [ onClick (ClickBranchHash hash) ]
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
        , text (String.fromInt (List.length children) ++ " edits")
        , column [] (List.map text edits)
        ]


viewRawCausal :
    HashDict Hash32 RawCausal
    -> RawCausal
    -> Element Message
viewRawCausal branches causal =
    viewRawBranch branches (rawCausalHead causal)


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
