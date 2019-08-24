module Ucb.Main.View.Type exposing (viewType)

import Element exposing (..)
import Ucb.Main.View.Reference exposing (viewReference)
import Ucb.Main.View.Symbol exposing (viewSymbol)
import Unison.Symbol exposing (..)
import Unison.Type exposing (..)


viewType :
    Type Symbol
    -> Element message
viewType { out } =
    case out of
        TypeVar var ->
            viewSymbol var

        TypeAbs var ty ->
            row
                [ spacing 2 ]
                [ viewSymbol var
                , text "."
                , viewType ty
                ]

        TypeTm (TypeRef ref) ->
            viewReference
                { showBuiltin = True
                , take = Just 7
                }
                ref

        TypeTm (TypeArrow ty1 ty2) ->
            row
                [ spacing 2 ]
                [ text "("
                , viewType ty1
                , text "->"
                , viewType ty2
                , text ")"
                ]

        TypeTm (TypeApp ty1 ty2) ->
            row
                [ spacing 2 ]
                [ text "("
                , viewType ty1
                , viewType ty2
                , text ")"
                ]

        TypeTm (TypeEffect ty1 ty2) ->
            row
                [ spacing 2 ]
                [ text "("
                , viewType ty1
                , viewType ty2
                , text ")"
                ]

        TypeTm (TypeEffects tys) ->
            row
                [ spacing 2 ]
                [ text "{"
                , row [] (List.map viewType tys)
                , text "}"
                ]

        TypeTm (TypeForall ty) ->
            row
                [ spacing 2 ]
                [ text "∀"
                , viewType ty
                ]

        TypeTm (TypeIntroOuter ty) ->
            viewType ty

        TypeCycle _ ->
            text "TypeCycle"

        TypeTm (TypeAnn _ _) ->
            text "TypeAnn"
