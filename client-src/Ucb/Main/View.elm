module Ucb.Main.View exposing (view)

import Browser exposing (Document)
import Element exposing (..)
import Element.Font as Font
import Element.Input exposing (labelLeft)
import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Html exposing (Html)
import Ucb.Main.Message exposing (..)
import Ucb.Main.Model exposing (..)
import Ucb.Main.View.Branch exposing (..)
import Ucb.Main.View.Palette exposing (mainFont)
import Ucb.Unison.NameSet as NameSet
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Name exposing (..)


view : Model -> Document Message
view model =
    { title = "Unison Code Browser"
    , body =
        [ layout [ width (fill |> minimum 800 |> maximum 1280), mainFont ]
            (view2 model)
        ]
    }


view2 : Model -> Element Message
view2 model =
    column [ spacing 80, padding 20, height fill, width fill ]
        [ header
        , row [ padding 20, spaceEvenly ]
            [ branches model
            , viewSearchPrototype model
            ]
        , errors model
        ]


branches : Model -> Element Message
branches model =
    column [ alignTop, width <| (fill |> minimum 500 |> maximum 1000) ] <|
        List.filterMap
            identity
            [ model.codebase.head
                |> Maybe.andThen
                    (\head ->
                        Maybe.map
                            (viewBranch model head)
                            (HashDict.get head model.codebase.branches)
                    )
            ]


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
    column [ alignTop, width <| (fill |> minimum 200 |> maximum 500), alignRight, spacing 10 ]
        [ Element.Input.text
            []
            { onChange = User_Search
            , text = model.ui.search
            , placeholder = Nothing
            , label = labelLeft [ centerX ] (text "Search")
            }
        , column [ scrollbarY, centerX, height (px 500) ] (List.map text matching)
        ]


errors : Model -> Element Message
errors model =
    if List.isEmpty model.errors then
        none

    else
        let
            errHeader =
                el [ Font.bold ] (text "Errors")
        in
        column [] <| errHeader :: List.map viewError (List.reverse model.errors)


header : Element msg
header =
    el [ centerX, Font.bold, Font.size 28 ] (Element.text "Unison Code Browser")


viewError : Error -> Element message
viewError error =
    text (Debug.toString error)