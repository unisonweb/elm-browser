module Ucb.Main.View exposing (viewModel)

import Browser exposing (Document)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button, labelLeft)
import HashingContainers.HashDict as HashDict
import HashingContainers.HashSet as HashSet
import Html exposing (Html)
import Misc exposing (maybe)
import Ucb.Main.Message exposing (..)
import Ucb.Main.Model exposing (..)
import Ucb.Main.View.Branch exposing (..)
import Ucb.Main.View.Palette exposing (mainFont)
import Ucb.Unison.NameSet as NameSet
import Ucb.Util.List as List
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.NameSegment exposing (..)
import Unison.Codebase.Patch exposing (..)
import Unison.Declaration exposing (..)
import Unison.Name exposing (..)
import Unison.Reference exposing (..)
import Unison.Symbol exposing (..)
import Unison.Term exposing (..)
import Unison.Type exposing (..)


type alias View =
    { branch : Branch
    , branchHash : BranchHash
    , errors : List Error
    , getTerm : Id -> Maybe (Term Symbol)
    , getTermType : Id -> Maybe (Type Symbol)
    , getTypeDecl : Id -> Maybe (Declaration Symbol)
    , hoveredTerm : Maybe Id
    , isBranchVisible : BranchHash -> Bool
    , isTermVisible : Id -> Bool
    , parents : BranchHash -> List BranchHash
    , patches : List ( PatchHash, Patch )
    , search : String
    , successors : BranchHash -> List BranchHash
    }


{-| Make a View from a Model. It returns Nothing if we haven't yet fetched the
head branch.
-}
makeViewFromModel :
    Model
    -> Maybe View
makeViewFromModel model =
    case model.codebase.head of
        Nothing ->
            Nothing

        Just head ->
            case HashDict.get head model.codebase.branches of
                Nothing ->
                    Nothing

                Just branch ->
                    Just (makeViewFromModel2 model head branch)


makeViewFromModel2 :
    Model
    -> BranchHash
    -> Branch
    -> View
makeViewFromModel2 model head branch =
    { branch = branch
    , branchHash = head
    , errors = model.errors
    , getTerm = \id -> HashDict.get id model.codebase.terms
    , getTermType = \id -> HashDict.get id model.codebase.termTypes
    , getTypeDecl = \id -> HashDict.get id model.codebase.typeDecls
    , hoveredTerm = model.ui.hoveredTerm
    , isBranchVisible = \hash -> HashDict.get hash model.ui.branches == Just True
    , isTermVisible = \id -> HashDict.get id model.ui.terms == Just True
    , parents = \hash -> maybe [] HashSet.toList (HashDict.get hash model.codebase.parents)
    , patches = HashDict.toList model.codebase.patches
    , search = model.ui.search
    , successors = \hash -> maybe [] HashSet.toList (HashDict.get hash model.codebase.successors)
    }


viewModel : Model -> Document Message
viewModel model =
    { title = "Unison Code Browser"
    , body =
        case makeViewFromModel model of
            Nothing ->
                []

            -- Loading
            Just view ->
                [ layout [ width (fill |> minimum 800 |> maximum 1280), mainFont ]
                    (viewView view)
                ]
    }


viewView : View -> Element Message
viewView view =
    column [ spacing 80, padding 20, height fill, width fill ]
        [ header
        , row [ padding 20, spaceEvenly ]
            [ viewBranches view
            , theBranch view
            , viewSearchPrototype view
            ]

        -- Debug info and WIP UI
        , el [ width fill, Border.width 1, Border.solid ] none
        , errors view
        , el [ width fill, Border.width 1, Border.solid ] none
        , viewPatchesPrototype view
        ]


{-| Branches sidebar.
-}
viewBranches :
    View
    -> Element message
viewBranches view =
    let
        branchNames : Branch -> List (List NameSegment)
        branchNames (Branch causal) =
            (rawCausalHead causal).children
                |> HashDict.toList
                |> List.sortBy Tuple.first
                |> List.concatMap
                    (\( name, ( _, branch ) ) ->
                        [ name ]
                            :: List.map (List.cons name) (branchNames branch)
                    )
    in
    column
        [ alignTop ]
        (branchNames view.branch
            |> List.cons []
            |> List.map (String.join "." >> String.cons '.')
            |> List.map text
        )


theBranch :
    View
    -> Element Message
theBranch view =
    column
        [ alignTop, width <| (fill |> minimum 500 |> maximum 1000) ]
        [ viewBranch view view.branchHash view.branch ]


viewPatchesPrototype :
    { r
        | branchHash : BranchHash
        , patches : List ( PatchHash, Patch )
    }
    -> Element Message
viewPatchesPrototype view =
    let
        viewPatch : ( PatchHash, Patch ) -> Element message
        viewPatch ( hash, { termEdits, typeEdits } ) =
            column
                []
                [ text "hash"
                , text hash
                , text "term edits"
                , text (Debug.toString termEdits)
                , text "type edits"
                , text (Debug.toString typeEdits)
                ]
    in
    column
        []
        [ button []
            { onPress = Just (User_GetPatches view.branchHash)
            , label = text "Get patches"
            }
        , text "patches:"
        , column []
            (List.map viewPatch view.patches)
        ]


viewSearchPrototype :
    { r
        | branch : Branch
        , search : String
    }
    -> Element Message
viewSearchPrototype view =
    let
        -- Term names, tossing the set of referent each is associated with (for
        -- now)
        termNames : List Name
        termNames =
            case view.branch of
                Branch causal ->
                    (rawCausalHead causal).cache.nameToTerm
                        |> HashDict.toList
                        |> List.map Tuple.first
                        |> NameSet.fromList
                        |> HashSet.toList

        -- Type names, tossing the set of reference each is associated with (for
        -- now)
        typeNames : List Name
        typeNames =
            case view.branch of
                Branch causal ->
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
            if String.isEmpty view.search then
                []

            else
                List.filterMap
                    (\string ->
                        if String.contains view.search (String.toLower string) then
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
            , text = view.search
            , placeholder = Nothing
            , label = labelLeft [ centerX ] (text "Search")
            }
        , column [ scrollbarY, centerX, height (px 500) ] (List.map text matching)
        ]


errors :
    { r | errors : List Error }
    -> Element Message
errors view =
    if List.isEmpty view.errors then
        none

    else
        let
            errHeader =
                el [ Font.bold ] (text "Errors")
        in
        column [] <| errHeader :: List.map viewError (List.reverse view.errors)


header : Element msg
header =
    el [ centerX, Font.bold, Font.size 28 ] (Element.text "Unison Code Browser")


viewError : Error -> Element message
viewError error =
    text (Debug.toString error)
