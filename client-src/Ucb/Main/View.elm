module Ucb.Main.View exposing (viewModel)

import Array
import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input exposing (button, labelLeft)
import HashingContainers.HashDict as HashDict
import HashingContainers.HashSet as HashSet
import Html exposing (Html)
import Misc exposing (impossible, maybe)
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
import Unison.Referent exposing (..)
import Unison.Symbol exposing (..)
import Unison.Term exposing (..)
import Unison.Type exposing (..)
import Util.HashSet as HashSet


type alias View =
    { branch : ( List NameSegment, ( BranchHash, Branch ) )
    , errors : List Error
    , getTerm : Id -> Maybe (Term Symbol)
    , getTermType : Id -> Maybe (Type Symbol)
    , getTypeDecl : Id -> Maybe (Declaration Symbol)
    , head : Branch
    , headHash : BranchHash
    , hoveredTerm : Maybe Id
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

        Just headHash ->
            case HashDict.get headHash model.codebase.branches of
                Nothing ->
                    Nothing

                Just head ->
                    Just (makeViewFromModel2 model headHash head)


makeViewFromModel2 :
    Model
    -> BranchHash
    -> Branch
    -> View
makeViewFromModel2 model headHash head =
    { branch =
        ( model.ui.branch
        , case model.ui.branch of
            [] ->
                ( headHash, head )

            path ->
                let
                    branch0 : Branch0
                    branch0 =
                        case head of
                            Branch branch ->
                                rawCausalHead branch
                in
                case HashDict.get (Array.fromList path) branch0.cache.pathToChild of
                    Nothing ->
                        impossible "missing child"

                    Just child ->
                        child
        )
    , errors = model.errors
    , getTerm = \id -> HashDict.get id model.codebase.terms
    , getTermType = \id -> HashDict.get id model.codebase.termTypes
    , getTypeDecl = \id -> HashDict.get id model.codebase.typeDecls
    , head = head
    , headHash = headHash
    , hoveredTerm = model.ui.hoveredTerm
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
            , el
                [ alignTop
                , width (fill |> minimum 500 |> maximum 1000)
                ]
                (case view.branch of
                    ( _, ( hash, branch ) ) ->
                        viewBranch view hash branch
                )
            , viewSearch view
            ]

        -- Debug info and WIP UI
        , el [ width fill, Border.width 1, Border.solid ] none
        , errors view
        , el [ width fill, Border.width 1, Border.solid ] none
        , viewPatches view
        ]


{-| Branches sidebar.
-}
viewBranches :
    View
    -> Element Message
viewBranches view =
    let
        branchInfo : List ( List NameSegment, Branch )
        branchInfo =
            ( [], view.head ) :: childInfo view.head

        childInfo :
            Branch
            -> List ( List NameSegment, Branch )
        childInfo (Branch branch) =
            (rawCausalHead branch).cache.pathToChild
                |> HashDict.toList
                |> List.sortWith
                    (\( n1, _ ) ( n2, _ ) -> nameCompare n1 n2)
                |> List.filterMap
                    (\( name, ( _, b ) ) ->
                        if shouldBeVisible b then
                            Just ( Array.toList name, b )

                        else
                            Nothing
                    )

        -- Should we show this branch?
        -- Yes if:
        -- * It contains any types
        -- * It contains any non-constructor terms
        shouldBeVisible :
            Branch
            -> Bool
        shouldBeVisible (Branch causal) =
            let
                branch0 : Branch0
                branch0 =
                    rawCausalHead causal
            in
            not (HashSet.isEmpty branch0.types.fact)
                || HashSet.foldl
                    (\referent acc -> referentIsReference referent || acc)
                    False
                    branch0.terms.fact

        viewInfo :
            ( List NameSegment, Branch )
            -> Element Message
        viewInfo ( path, branch ) =
            el
                [ if path == Tuple.first view.branch then
                    Background.color (rgb 0.5 0.5 0.5)

                  else
                    Background.color (rgb 1 1 1)
                , onClick (User_ClickBranch path branch)
                , pointer
                ]
                (text (String.cons '.' (String.join "." path)))
    in
    column
        [ alignTop
        , width (fill |> minimum 200)
        ]
        (List.map viewInfo branchInfo)


viewPatches :
    { r
        | headHash : BranchHash
        , patches : List ( PatchHash, Patch )
    }
    -> Element Message
viewPatches view =
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
            { onPress = Just (User_GetPatches view.headHash)
            , label = text "Get patches"
            }
        , text "patches:"
        , column []
            (List.map viewPatch view.patches)
        ]


viewSearch :
    { r
        | head : Branch
        , search : String
    }
    -> Element Message
viewSearch view =
    let
        -- Term names, tossing the set of referent each is associated with (for
        -- now)
        termNames : List Name
        termNames =
            case view.head of
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
            case view.head of
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
