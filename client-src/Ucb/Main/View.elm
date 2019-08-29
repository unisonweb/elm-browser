module Ucb.Main.View exposing (..)

import Browser exposing (Document)
import Element exposing (..)
import Element.Input exposing (labelLeft)
import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Html exposing (Html)
import Ucb.Main.Message exposing (..)
import Ucb.Main.Model exposing (..)
import Ucb.Main.View.Branch exposing (..)
import Ucb.Unison.NameSet as NameSet
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Name exposing (..)


view : Model -> Document Message
view model =
    { title = "Unison Code Browser", body = [ layout [] (view2 model) ] }


view2 : Model -> Element Message
view2 model =
    column
        []
        (List.filterMap identity
            [ model.codebase.head
                |> Maybe.andThen
                    (\head ->
                        Maybe.map
                            (viewBranch model head)
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
            , Just (viewSearchPrototype model)
            ]
        )


viewSearchPrototype :
    Model
    -> Element Message
viewSearchPrototype model =
    let
        maybeBranch : Maybe Branch
        maybeBranch =
            model.codebase.head
                |> Maybe.andThen
                    (\head ->
                        HashDict.get head model.codebase.branches
                    )

        -- Term names, tossing the set of referent each is associated with (for
        -- now)
        termNames : List Name
        termNames =
            case maybeBranch of
                Nothing ->
                    []

                Just (Branch causal) ->
                    (rawCausalHead causal).cache.nameToTerm
                        |> HashDict.toList
                        |> List.map Tuple.first
                        |> NameSet.fromList
                        |> HashSet.toList

        -- Type names, tossing the set of reference each is associated with (for
        -- now)
        typeNames : List Name
        typeNames =
            case maybeBranch of
                Nothing ->
                    []

                Just (Branch causal) ->
                    (rawCausalHead causal).cache.nameToTerm
                        |> HashDict.toList
                        |> List.map Tuple.first
                        |> NameSet.fromList
                        |> HashSet.toList

        names : List Name
        names =
            typeNames ++ termNames

        strings : List String
        strings =
            List.map nameToString names

        matching : List String
        matching =
            if String.isEmpty model.ui.search then
                []

            else
                List.filterMap
                    (\string ->
                        if String.contains model.ui.search (String.toLower string) then
                            Just string

                        else
                            Nothing
                    )
                    strings
    in
    column
        []
        [ text "Hi, I'm your friendly neighborhood search box"
        , text "I was implemented by a backend developer so I'm a bit visually challenged"
        , text "Anywho please give me a spin!"
        , Element.Input.text
            []
            { onChange = User_Search
            , text = model.ui.search
            , placeholder = Nothing
            , label = labelLeft [] (text "Search")
            }
        , column [] (List.map text matching)
        ]


viewError :
    Error
    -> Element message
viewError error =
    text (Debug.toString error)