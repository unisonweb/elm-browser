module Ucb.Main.View.Type exposing (viewType)

import Element exposing (..)
import HashingContainers.HashDict as HashDict
import HashingContainers.HashSet as HashSet
import Ucb.Main.Model exposing (..)
import Ucb.Main.View.Reference exposing (viewReference)
import Ucb.Main.View.Symbol exposing (viewSymbol)
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Symbol exposing (..)
import Unison.Type exposing (..)


viewType :
    Model
    -> Type Symbol
    -> Element message
viewType model { out } =
    case out of
        TypeVar var ->
            viewSymbol var

        TypeAbs var ty ->
            row
                [ spacing 2 ]
                [ viewSymbol var
                , text "."
                , viewType model ty
                ]

        TypeTm (TypeRef reference) ->
            let
                fallback : Element message
                fallback =
                    viewReference
                        { showBuiltin = True
                        , take = Just 7
                        }
                        reference
            in
            case model.codebase.head of
                Nothing ->
                    fallback

                Just hash ->
                    case HashDict.get hash model.codebase.branches of
                        Nothing ->
                            fallback

                        Just (Branch causal) ->
                            case HashDict.get reference (rawCausalHead causal).cache.typeNames.domain of
                                Nothing ->
                                    fallback

                                Just names ->
                                    case HashSet.toList names of
                                        -- ???
                                        [] ->
                                            fallback

                                        -- Horrible monster, just want to show
                                        -- all the aliases for now, rather than
                                        -- arbitrarily pick one
                                        names2 ->
                                            names2
                                                |> List.map (String.join ".")
                                                |> String.join "∕"
                                                |> text

        TypeTm (TypeArrow ty1 ty2) ->
            row
                [ spacing 2 ]
                [ text "("
                , viewType model ty1
                , text "->"
                , viewType model ty2
                , text ")"
                ]

        TypeTm (TypeApp ty1 ty2) ->
            row
                [ spacing 2 ]
                [ text "("
                , viewType model ty1
                , viewType model ty2
                , text ")"
                ]

        TypeTm (TypeEffect ty1 ty2) ->
            row
                [ spacing 2 ]
                [ text "("
                , viewType model ty1
                , viewType model ty2
                , text ")"
                ]

        TypeTm (TypeEffects tys) ->
            row
                [ spacing 2 ]
                [ text "{"
                , row [] (List.map (viewType model) tys)
                , text "}"
                ]

        TypeTm (TypeForall ty) ->
            row
                [ spacing 2 ]
                [ text "∀"
                , viewType model ty
                ]

        TypeTm (TypeIntroOuter ty) ->
            viewType model ty

        TypeCycle _ ->
            text "TypeCycle"

        TypeTm (TypeAnn _ _) ->
            text "TypeAnn"
