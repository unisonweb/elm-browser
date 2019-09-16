module Ucb.Main.View.Branch exposing (viewBranch)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import HashingContainers.HashDict as HashDict
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Misc exposing (maybe)
import Typeclasses.Classes.Equality as Equality
import Typeclasses.Classes.Hashing as Hashing
import Ucb.Main.Message exposing (..)
import Ucb.Main.Model exposing (..)
import Ucb.Main.View.Palette exposing (codeFont, hashColor, hoverStyle)
import Ucb.Main.View.Term exposing (viewTerm)
import Ucb.Main.View.Type exposing (viewType)
import Ucb.Unison.VType exposing (..)
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.NameSegment exposing (..)
import Unison.ConstructorType exposing (..)
import Unison.Declaration exposing (..)
import Unison.Name exposing (..)
import Unison.Reference exposing (..)
import Unison.Referent exposing (..)
import Unison.Symbol exposing (..)
import Unison.Term exposing (..)
import Unison.Type exposing (..)
import Unison.Util.Relation exposing (..)


{-| TODO(elliott) pass in this branch's path
-}
viewBranch :
    { r
        | getTerm : Id -> Maybe (Term Symbol)
        , getTermType : Id -> Maybe (Type Symbol)
        , getTypeDecl : Id -> Maybe (Declaration Symbol)
        , head : Branch
        , hovered : Maybe Hover
        , isTermVisible : Id -> Bool
        , parents : BranchHash -> List BranchHash
        , successors : BranchHash -> List BranchHash
        , termNames : Referent -> List Name
        , typeNames : Reference -> List Name
    }
    -> BranchHash
    -> Branch
    -> Element Message
viewBranch view hash (Branch causal) =
    viewCausal view hash causal


viewBranch0 :
    { r
        | getTerm : Id -> Maybe (Term Symbol)
        , getTermType : Id -> Maybe (Type Symbol)
        , getTypeDecl : Id -> Maybe (Declaration Symbol)
        , head : Branch
        , hovered : Maybe Hover
        , isTermVisible : Id -> Bool
        , parents : BranchHash -> List BranchHash
        , successors : BranchHash -> List BranchHash
        , termNames : Referent -> List Name
        , typeNames : Reference -> List Name
    }
    -> Branch0
    -> Element Message
viewBranch0 view { terms, types, children, patches } =
    column
        [ spacing 30 ]
        [ case relationToList types.d1 of
            [] ->
                none

            types2 ->
                column [ spacing 20 ]
                    (types2
                        |> List.sortBy Tuple.second
                        |> List.indexedMap
                            (\i ( reference, name ) ->
                                viewBranchType
                                    view
                                    i
                                    reference
                                    name
                                    (HashDict.get reference types.d3.domain)
                            )
                    )
        , case relationToList terms.d1 of
            [] ->
                none

            terms2 ->
                column
                    [ spacing 20 ]
                    (terms2
                        |> List.sortBy Tuple.second
                        |> List.indexedMap
                            (\i ( referent, name ) ->
                                viewBranchTerm
                                    view
                                    i
                                    referent
                                    name
                                    (HashDict.get referent terms.d3.domain)
                            )
                    )
        , -- TODO
          case patches of
            _ ->
                none
        ]


{-| View a term in a branch.
-}
viewBranchTerm :
    { r
        | getTerm : Id -> Maybe (Term Symbol)
        , getTermType : Id -> Maybe (Type Symbol)
        , head : Branch
        , hovered : Maybe Hover
        , isTermVisible : Id -> Bool
        , termNames : Referent -> List Name
        , typeNames : Reference -> List Name
    }
    -> Int
    -> Referent
    -> NameSegment
    -> Maybe (HashSet ( Reference, Reference ))
    -> Element Message
viewBranchTerm view i referent name links =
    case referent of
        Ref reference ->
            viewBranchTerm2 view i reference name links

        Con _ _ _ ->
            none


viewBranchTerm2 :
    { r
        | getTerm : Id -> Maybe (Term Symbol)
        , getTermType : Id -> Maybe (Type Symbol)
        , head : Branch
        , hovered : Maybe Hover
        , isTermVisible : Id -> Bool
        , termNames : Referent -> List Name
        , typeNames : Reference -> List Name
    }
    -> Int
    -> Reference
    -> NameSegment
    -> Maybe (HashSet ( Reference, Reference ))
    -> Element Message
viewBranchTerm2 view i reference name _ =
    let
        -- Surround by parens if it begins with a symboly character
        name2 : String
        name2 =
            case String.uncons name of
                Just ( c, _ ) ->
                    if HashSet.member c symbolyIdChars then
                        "(" ++ name ++ ")"

                    else
                        name

                Nothing ->
                    name
    in
    case reference of
        Builtin _ ->
            el
                [ above <|
                    if view.hovered == Just (HoverTerm reference) then
                        el
                            hoverStyle
                            (column
                                []
                                (List.map
                                    (nameToString >> text)
                                    (view.termNames (Ref reference))
                                )
                            )

                    else
                        none
                , codeFont
                , onMouseEnter (User_Hover (HoverTerm reference))
                , onMouseLeave User_Unhover
                ]
                (text name2)

        Derived id ->
            column []
                [ row
                    [ codeFont
                    ]
                    [ el
                        [ above <|
                            if view.hovered == Just (HoverTerm reference) then
                                el
                                    hoverStyle
                                    (column
                                        []
                                        (el
                                            [ hashColor ]
                                            (text id.hash)
                                            :: List.map
                                                (nameToString >> text)
                                                (view.termNames (Ref reference))
                                        )
                                    )

                            else
                                none
                        , onClick (User_ToggleTerm id)
                        , onMouseEnter (User_Hover (HoverTerm reference))
                        , onMouseLeave User_Unhover
                        , pointer
                        ]
                        (text name2)
                    , maybe
                        none
                        (\type_ ->
                            row []
                                [ text " : "
                                , viewType view [ i, 1 ] -1 (makeVType type_)
                                ]
                        )
                        (view.getTermType id)
                    ]
                , if view.isTermVisible id then
                    maybe
                        none
                        (\term ->
                            el
                                [ codeFont
                                , paddingEach { bottom = 5, left = 10, right = 0, top = 5 }
                                ]
                                (viewTerm view term)
                        )
                        (view.getTerm id)

                  else
                    none
                ]


{-| View a type in a branch.
-}
viewBranchType :
    { r
        | getTypeDecl : Id -> Maybe (Declaration Symbol)
        , head : Branch
        , hovered : Maybe Hover
        , typeNames : Reference -> List Name
    }
    -> Int
    -> Reference
    -> NameSegment
    -> Maybe (HashSet ( Reference, Reference ))
    -> Element Message
viewBranchType view i reference name links =
    case reference of
        Builtin _ ->
            row
                [ codeFont
                ]
                [ el [ Font.bold ] (text "unique type ")
                , text name
                ]

        Derived id ->
            case view.getTypeDecl id of
                Nothing ->
                    none

                Just declaration ->
                    case declaration of
                        DataDecl dataDeclaration ->
                            viewBranchType2 view i name links id dataDeclaration Data

                        EffectDecl dataDeclaration ->
                            viewBranchType2 view i name links id dataDeclaration Effect


viewBranchType2 :
    { r
        | head : Branch
        , hovered : Maybe Hover
        , typeNames : Reference -> List Name
    }
    -> Int
    -> NameSegment
    -> Maybe (HashSet ( Reference, Reference ))
    -> Id
    -> DataDeclaration Symbol
    -> ConstructorType
    -> Element Message
viewBranchType2 view i name _ _ declaration constructorType =
    column
        []
        [ row
            [ codeFont ]
            [ el [ Font.bold ]
                (case constructorType of
                    Data ->
                        case declaration.modifier of
                            Structural ->
                                text "structural type "

                            Unique _ ->
                                text "unique type "

                    Effect ->
                        case declaration.modifier of
                            Structural ->
                                text "structural ability "

                            Unique _ ->
                                text "unique ability "
                )
            , text (String.join " " (name :: List.map symbolToString declaration.bound))
            ]
        , el
            [ paddingEach { bottom = 5, left = 10, right = 0, top = 5 } ]
            (column
                [ codeFont
                , spacing 5
                ]
                (List.indexedMap
                    (\j ( constructorName, type_ ) ->
                        row
                            []
                            [ text (symbolToString constructorName ++ " : ")
                            , viewType view [ j, i, 0 ] -1 (makeVType type_)
                            ]
                    )
                    declaration.constructors
                )
            )
        ]


viewCausal :
    { r
        | getTerm : Id -> Maybe (Term Symbol)
        , getTermType : Id -> Maybe (Type Symbol)
        , getTypeDecl : Id -> Maybe (Declaration Symbol)
        , head : Branch
        , hovered : Maybe Hover
        , isTermVisible : Id -> Bool
        , parents : BranchHash -> List BranchHash
        , successors : BranchHash -> List BranchHash
        , termNames : Referent -> List Name
        , typeNames : Reference -> List Name
    }
    -> BranchHash
    -> RawCausal Branch0
    -> Element Message
viewCausal view hash causal =
    let
        viewHash : BranchHash -> Element Message
        viewHash hash_ =
            el
                [ pointer -- onClick (User_FocusBranch hash_)
                -- , pointer
                ]
                -- TODO show full hash on hover
                (link [] {url = "/branch/" ++ hash_, label = text (String.left 7 hash_)})

        viewParents : Element Message
        viewParents =
            case view.parents hash of
                [] ->
                    none

                hashes ->
                    row
                        []
                        [ text "Parents "
                        , column [] (List.map viewHash hashes)
                        ]

        viewPredecessors :
            List BranchHash
            -> Element Message
        viewPredecessors hashes =
            row
                []
                [ text "Predecessors "
                , column [] (List.map viewHash hashes)
                ]

        viewSuccessors : Element Message
        viewSuccessors =
            case view.successors hash of
                [] ->
                    none

                hashes ->
                    row
                        []
                        [ text "Successors "
                        , column [] (List.map viewHash hashes)
                        ]
    in
    el [ padding 10 ]
        (case causal of
            RawOne branch ->
                column
                    [ spacing 40 ]
                    [ column []
                        [ viewHash hash
                        , viewParents
                        , viewSuccessors
                        ]
                    , viewBranch0 view branch
                    ]

            RawCons branch hash_ ->
                column
                    [ spacing 40 ]
                    [ column []
                        [ viewHash hash
                        , viewParents
                        , viewPredecessors [ hash_ ]
                        , viewSuccessors
                        ]
                    , viewBranch0 view branch
                    ]

            RawMerge branch hashes ->
                column
                    [ spacing 40 ]
                    [ column []
                        [ viewHash hash
                        , viewParents
                        , viewPredecessors (HashSet.toList hashes)
                        , viewSuccessors
                        ]
                    , viewBranch0 view branch
                    ]
        )


symbolyIdChars : HashSet Char
symbolyIdChars =
    HashSet.fromList
        Equality.char
        Hashing.char
        [ '!'
        , '$'
        , '%'
        , '^'
        , '&'
        , '*'
        , '-'
        , '='
        , '+'
        , '<'
        , '>'
        , '.'
        , '~'
        , '\\'
        , '/'
        , '|'
        , ':'
        ]
