module Ucb.Main.View.Type exposing (viewType)

import Element exposing (..)
import HashingContainers.HashDict as HashDict
import HashingContainers.HashSet as HashSet
import Misc exposing (..)
import Ucb.Main.Model exposing (..)
import Ucb.Main.View.Reference exposing (viewReference)
import Ucb.Main.View.Symbol exposing (viewSymbol)
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Symbol exposing (..)
import Unison.Type exposing (..)


viewType :
    Model
    -> Int
    -> Type Symbol
    -> Element message
viewType model p ty0 =
    case ty0.out of
        TypeVar var ->
            viewSymbol var

        TypeAbs var ty ->
            row
                [ spacing 2 ]
                [ viewSymbol var
                , text "."
                , viewType model p ty
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

                                        [ name ] ->
                                            name
                                                |> listLast
                                                |> Maybe.withDefault "???"
                                                |> text

                                        names2 ->
                                            names2
                                                |> List.map (String.join ".")
                                                |> String.join "∕"
                                                |> text

        TypeTm (TypeArrow ty1 ty2) ->
            row
                [ spacing 2 ]
                [ text "("
                , viewType model p ty1
                , text "->"
                , viewType model p ty2
                , text ")"
                ]

        TypeTm (TypeApp ty1 ty2) ->
            row
                [ spacing 2 ]
                [ text "("
                , viewType model p ty1
                , viewType model p ty2
                , text ")"
                ]

        TypeTm (TypeEffect ty1 ty2) ->
            row
                [ spacing 2 ]
                [ text "("
                , viewType model p ty1
                , viewType model p ty2
                , text ")"
                ]

        TypeTm (TypeEffects tys) ->
            row
                [ spacing 2 ]
                [ text "{"
                , row [] (List.map (viewType model p) tys)
                , text "}"
                ]

        TypeTm (TypeForall _) ->
            let
                ( tyvars, ty ) =
                    unForalls [] ty0
            in
            paren (p >= 0)
                (row
                    [ spacing 2 ]
                    [ text (String.join " " ("∀" :: List.map symbolToString tyvars) ++ ". ")
                    , viewType model -1 ty
                    ]
                )

        TypeTm (TypeIntroOuter ty) ->
            viewType model p ty

        TypeCycle _ ->
            text "TypeCycle"

        TypeTm (TypeAnn _ _) ->
            text "TypeAnn"


paren : Bool -> Element message -> Element message
paren b x =
    if b then
        row [] [ text "(", x, text ")" ]

    else
        x


unForalls : List var -> Type var -> ( List var, Type var )
unForalls vars ty =
    case ty.out of
        TypeTm (TypeForall ty2) ->
            case ty2.out of
                TypeAbs var ty3 ->
                    unForalls (var :: vars) ty3

                _ ->
                    impossible "unForalls: forall not followed by abs"

        _ ->
            ( List.reverse vars, ty )
