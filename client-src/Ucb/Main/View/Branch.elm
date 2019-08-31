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
import Ucb.Main.Model exposing (..)
import Ucb.Main.View.Palette exposing (codeFont)
import Ucb.Main.View.Reference exposing (viewId, viewReference)
import Ucb.Main.View.Referent exposing (viewReferent)
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
import Unison.Util.Relation exposing (..)


viewBranch :
    Model
    -> BranchHash
    -> Branch
    -> Element Message
viewBranch model hash (Branch causal) =
    viewCausal model hash causal


viewBranch0 :
    Model
    -> Branch0
    -> Element Message
viewBranch0 model { terms, types, children, edits, cache } =
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
                                    model
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
                                    model
                                    referent
                                    name
                                    (HashDict.get referent terms.d3.domain)
                            )
                    )
        , case HashDict.toList children of
            [] ->
                none

            children2 ->
                column
                    [ spacing 20 ]
                    (children2
                        |> List.sortBy Tuple.first
                        |> List.map
                            (\( name, ( hash, branch ) ) ->
                                viewBranchChild model name hash branch
                            )
                    )

        -- , column [] (List.map text edits)
        ]


{-| View a child branch.
-}
viewBranchChild :
    Model
    -> NameSegment
    -> BranchHash
    -> Branch
    -> Element Message
viewBranchChild model name hash branch =
    column
        []
        [ el
            [ onClick (User_ToggleBranch hash)
            , pointer
            ]
            (row
                []
                [ el [ bold ] (text "child ")
                , text name
                ]
            )
        , case HashDict.get hash model.ui.branches of
            Just True ->
                el
                    [ paddingEach { bottom = 0, left = 10, right = 0, top = 0 } ]
                    (viewBranch model hash branch)

            _ ->
                none
        ]


{-| View a term in a branch.
-}
viewBranchTerm :
    Model
    -> Referent
    -> NameSegment
    -> Maybe (HashSet ( Reference, Reference ))
    -> Element Message
viewBranchTerm model referent name links =
    case referent of
        Ref reference ->
            viewBranchTerm2 model reference name links

        Con _ _ _ ->
            none


viewBranchTerm2 :
    Model
    -> Reference
    -> NameSegment
    -> Maybe (HashSet ( Reference, Reference ))
    -> Element Message
viewBranchTerm2 model reference name links =
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
                    ]
                    [ text name2
                    , maybe
                        none
                        (\type_ ->
                            row []
                                [ text " : "
                                , viewType model -1 type_
                                ]
                        )
                        (HashDict.get id model.codebase.termTypes)
                    ]
                , case HashDict.get id model.ui.terms of
                    Just True ->
                        maybe
                            none
                            (\term ->
                                el
                                    [ codeFont
                                    , paddingEach { bottom = 5, left = 10, right = 0, top = 5 }
                                    ]
                                    (viewTerm model term)
                            )
                            (HashDict.get id model.codebase.terms)

                    _ ->
                        none
                ]


{-| View a type in a branch.
-}
viewBranchType :
    Model
    -> Reference
    -> NameSegment
    -> Maybe (HashSet ( Reference, Reference ))
    -> Element message
viewBranchType model reference name links =
    case reference of
        Builtin _ ->
            row
                [ codeFont
                ]
                [ el [ bold ] (text "unique type ")
                , text name
                ]

        Derived id ->
            case HashDict.get id model.codebase.typeDecls of
                Nothing ->
                    none

                Just declaration ->
                    case declaration of
                        DataDecl dataDeclaration ->
                            viewBranchType2 model name links id dataDeclaration Data

                        EffectDecl dataDeclaration ->
                            viewBranchType2 model name links id dataDeclaration Effect


viewBranchType2 :
    Model
    -> NameSegment
    -> Maybe (HashSet ( Reference, Reference ))
    -> Id
    -> DataDeclaration Symbol
    -> ConstructorType
    -> Element message
viewBranchType2 model name links id declaration constructorType =
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
                            , viewType model -1 type_
                            ]
                    )
                    declaration.constructors
                )
            )
        ]


viewCausal :
    Model
    -> BranchHash
    -> RawCausal Branch0
    -> Element Message
viewCausal model hash causal =
    let
        viewHash : BranchHash -> Element Message
        viewHash hash_ =
            el
                [ onClick (User_FocusBranch hash_)
                , pointer
                , width (px 100)
                , clipX
                ]
                (text hash_)

        viewParents : Element Message
        viewParents =
            case HashDict.get hash model.codebase.parents of
                Nothing ->
                    none

                Just hashes ->
                    row
                        []
                        [ text "Parents "
                        , column [] (List.map viewHash (HashSet.toList hashes))
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
            case HashDict.get hash model.codebase.successors of
                Nothing ->
                    none

                Just hashes ->
                    row
                        []
                        [ text "Successors "
                        , column [] (List.map viewHash (HashSet.toList hashes))
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
                    , viewBranch0 model branch
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
                    , viewBranch0 model branch
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
                    , viewBranch0 model branch
                    ]
        )


viewLinks :
    Maybe (HashSet ( Reference, Reference ))
    -> Element message
viewLinks maybeLinks =
    case maybeLinks of
        Nothing ->
            none

        Just links ->
            row [ spacing 5 ]
                (text "links"
                    :: List.map
                        (Tuple.second
                            >> viewReference
                                { showBuiltin = True
                                , take = Just 7
                                }
                        )
                        (HashSet.toList links)
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
