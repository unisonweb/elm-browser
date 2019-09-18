module Ucb.Main.View exposing (viewModel)

import Array
import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input exposing (button)
import HashingContainers.HashDict as HashDict
import HashingContainers.HashSet as HashSet
import Html
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
    , hovered : Maybe Hover
    , isTermVisible : Id -> Bool
    , parents : BranchHash -> List BranchHash
    , patches : List ( PatchHash, Patch )
    , search : String
    , successors : BranchHash -> List BranchHash
    , termNames : Referent -> List Name
    , typeNames : Reference -> List Name
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
                        branchHead head
                in
                case HashDict.get (unsafeMakeName (Array.fromList path)) branch0.cache.pathToChild of
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
    , hovered = model.ui.hovered
    , isTermVisible =
        \id ->
            HashDict.get id model.ui.terms == Just True
    , parents =
        \hash ->
            maybe
                []
                HashSet.toList
                (HashDict.get hash model.codebase.parents)
    , patches = HashDict.toList model.codebase.patches
    , search = model.ui.search
    , successors =
        \hash ->
            maybe
                []
                HashSet.toList
                (HashDict.get hash model.codebase.successors)
    , termNames =
        \referent ->
            maybe
                []
                (HashSet.toList >> List.sortWith nameCompare)
                (HashDict.get referent (branchHead head).cache.termToName)
    , typeNames =
        \reference ->
            maybe
                []
                (HashSet.toList >> List.sortWith nameCompare)
                (HashDict.get reference (branchHead head).cache.typeToName)
    }


viewModel : Model -> Document Message
viewModel model =
    { title = "Unison Code Browser"
    , body =
        case makeViewFromModel model of
            Nothing ->
                [ Html.text "Loading..." ]

            -- Loading
            Just view ->
                [ layout [ mainFont ]
                    (el
                        [ centerX, width (fill |> minimum 800 |> maximum 1280) ]
                        (viewView view)
                    )
                ]
    }


viewView : View -> Element Message
viewView view =
    column
        [ centerX
        , width fill
        , height fill
        , spacing 80
        , padding 20
        ]
        [ header

        -- main body
        , row [ padding 20, width fill ]
            [ el [ alignTop, width <| fillPortion 2, scrollbars ]
                (case view.branch of
                    ( _, ( hash, branch ) ) ->
                        viewBranch view hash branch
                )
            , column
                [ alignTop
                , width <| fillPortion 1
                , spacing 10
                , padding 20
                , Border.solid
                , Border.widthEach { bottom = 0, left = 1, top = 0, right = 0 }
                ]
                [ viewSearch view

                -- spacer
                , el [ Border.solid, Border.width 1, width fill ] none
                , viewBranches view
                ]
            ]

        -- Debug info and WIP UI
        , el [ width fill, Border.width 1, Border.solid ] none
        , errors view
        , el [ width fill, Border.width 1, Border.solid ] none
        , viewPatches view
        ]


{-| Branches sidebar.
-}
viewBranches : View -> Element Message
viewBranches view =
    let
        branchInfo : List ( List NameSegment, Branch )
        branchInfo =
            (( [], view.head ) :: childInfo view.head)
                |> List.filter (Tuple.second >> shouldBeVisible)

        childInfo :
            Branch
            -> List ( List NameSegment, Branch )
        childInfo branch =
            (branchHead branch).cache.pathToChild
                |> HashDict.toList
                |> List.sortWith
                    (\( n1, _ ) ( n2, _ ) -> nameCompare n1 n2)
                |> List.map
                    (\( name, ( _, b ) ) ->
                        ( Array.toList (nameToNameSegments name), b )
                    )

        -- Should we show this branch?
        -- Yes if:
        -- * It contains any types
        -- * It contains any non-constructor terms
        shouldBeVisible :
            Branch
            -> Bool
        shouldBeVisible branch =
            let
                branch0 : Branch0
                branch0 =
                    branchHead branch
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
            (branchHead view.head).cache.nameToTerm
                |> HashDict.toList
                |> List.map Tuple.first
                |> NameSet.fromList
                |> HashSet.toList

        -- Type names, tossing the set of reference each is associated with (for
        -- now)
        typeNames : List Name
        typeNames =
            (branchHead view.head).cache.nameToType
                |> HashDict.toList
                |> List.map Tuple.first
                |> NameSet.fromList
                |> HashSet.toList

        matchingNames : List Name
        matchingNames =
            if String.isEmpty view.search then
                []

            else
                List.filterMap
                    (\name ->
                        if String.contains view.search (nameLast name) then
                            Just name

                        else
                            Nothing
                    )
                    (typeNames ++ termNames)
    in
    column
        [ scrollbarY
        , centerX
        , width fill
        , height (fill |> maximum 500)
        , spacing 10
        ]
        [ Element.Input.text
            []
            { onChange = User_Search
            , text = view.search
            , placeholder = Just <| Element.Input.placeholder [] (text "Search")
            , label = Element.Input.labelHidden "Search"
            }
        , column [ scrollbarY, centerX, height (fill |> maximum 500), width fill ]
            (List.map (nameToString >> text) matchingNames)
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
