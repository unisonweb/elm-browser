module Ucb.Main.View.Branch exposing (viewBranch)

import Element exposing (..)
import Element.Events exposing (..)
import Element.Font exposing (..)
import HashingContainers.HashDict as HashDict
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Misc exposing (maybe)
import Typeclasses.Classes.Equality as Equality
import Typeclasses.Classes.Hashing as Hashing
import Ucb.Main.Message exposing (..)
import Ucb.Main.View.Palette exposing (codeFont)
import Ucb.Main.View.Term exposing (viewTerm)
import Ucb.Main.View.Type exposing (viewType)
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.NameSegment exposing (..)
import Unison.ConstructorType exposing (..)
import Unison.Declaration exposing (..)
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
        , hoveredTerm : Maybe Id
        , isTermVisible : Id -> Bool
        , parents : BranchHash -> List BranchHash
        , successors : BranchHash -> List BranchHash
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
        , hoveredTerm : Maybe Id
        , isTermVisible : Id -> Bool
        , parents : BranchHash -> List BranchHash
        , successors : BranchHash -> List BranchHash
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
                        |> List.map
                            (\( reference, name ) ->
                                viewBranchType
                                    view
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
                        |> List.map
                            (\( referent, name ) ->
                                viewBranchTerm
                                    view
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
        , hoveredTerm : Maybe Id
        , isTermVisible : Id -> Bool
    }
    -> Referent
    -> NameSegment
    -> Maybe (HashSet ( Reference, Reference ))
    -> Element Message
viewBranchTerm view referent name links =
    case referent of
        Ref reference ->
            viewBranchTerm2 view reference name links

        Con _ _ _ ->
            none


viewBranchTerm2 :
    { r
        | getTerm : Id -> Maybe (Term Symbol)
        , getTermType : Id -> Maybe (Type Symbol)
        , head : Branch
        , hoveredTerm : Maybe Id
        , isTermVisible : Id -> Bool
    }
    -> Reference
    -> NameSegment
    -> Maybe (HashSet ( Reference, Reference ))
    -> Element Message
viewBranchTerm2 view reference name _ =
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
                [ codeFont ]
                (text name2)

        Derived id ->
            column []
                [ row
                    [ codeFont
                    , onClick (User_ToggleTerm id)
                    , pointer
                    , onMouseEnter (User_HoverTerm id)
                    , onMouseLeave User_LeaveTerm
                    , below <|
                        if Just id == view.hoveredTerm then
                            -- TODO find the full name and print it
                            el [] none

                        else
                            none
                    ]
                    [ text name2
                    , maybe
                        none
                        (\type_ ->
                            row []
                                [ text " : "
                                , viewType view -1 type_
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
    }
    -> Reference
    -> NameSegment
    -> Maybe (HashSet ( Reference, Reference ))
    -> Element message
viewBranchType view reference name links =
    case reference of
        Builtin _ ->
            row
                [ codeFont
                ]
                [ el [ bold ] (text "unique type ")
                , text name
                ]

        Derived id ->
            case view.getTypeDecl id of
                Nothing ->
                    none

                Just declaration ->
                    case declaration of
                        DataDecl dataDeclaration ->
                            viewBranchType2 view name links id dataDeclaration Data

                        EffectDecl dataDeclaration ->
                            viewBranchType2 view name links id dataDeclaration Effect


viewBranchType2 :
    { r | head : Branch }
    -> NameSegment
    -> Maybe (HashSet ( Reference, Reference ))
    -> Id
    -> DataDeclaration Symbol
    -> ConstructorType
    -> Element message
viewBranchType2 view name _ _ declaration constructorType =
    column
        []
        [ row
            [ codeFont ]
            [ el [ bold ]
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
                (List.map
                    (\( constructorName, type_ ) ->
                        row
                            []
                            [ text (symbolToString constructorName ++ " : ")
                            , viewType view -1 type_
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
        , hoveredTerm : Maybe Id
        , isTermVisible : Id -> Bool
        , parents : BranchHash -> List BranchHash
        , successors : BranchHash -> List BranchHash
    }
    -> BranchHash
    -> RawCausal Branch0
    -> Element Message
viewCausal view hash causal =
    let
        viewHash : BranchHash -> Element Message
        viewHash hash_ =
            el
                [ onClick (User_FocusBranch hash_)
                , pointer
                ]
                -- TODO show full hash on hover
                (text (String.left 7 hash_))

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
