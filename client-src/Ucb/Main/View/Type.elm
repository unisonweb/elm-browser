module Ucb.Main.View.Type exposing (viewType)

import Element exposing (..)
import HashingContainers.HashDict as HashDict
import HashingContainers.HashSet as HashSet
import Misc exposing (..)
import Ucb.Main.Model exposing (..)
import Ucb.Main.View.Reference exposing (viewReference)
import Ucb.Util.Pretty exposing (..)
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.NameSegment exposing (..)
import Unison.Reference exposing (..)
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
            text (symbolToString var)

        TypeTm (TypeRef reference) ->
            viewTypeRef model reference

        TypeTm (TypeArrow ty1 ty2) ->
            ppParen (p >= 0)
                (row
                    []
                    [ viewType model 0 ty1
                    , viewArrows model (unEffectfulArrows ty2)
                    ]
                )

        TypeTm (TypeApp ty1 ty2) ->
            case ty1.out of
                TypeTm (TypeRef (Builtin "Sequence")) ->
                    row
                        []
                        [ text "[", viewType model 0 ty2, text "]" ]

                _ ->
                    case unApps ty0 of
                        Nothing ->
                            impossible "viewType: unApps returned Nothing"

                        Just ( f, xs ) ->
                            ppParen (p >= 10)
                                (row
                                    []
                                    [ viewType model 9 f
                                    , text " "
                                    , ppSpaced (List.map (viewType model 10) xs)
                                    ]
                                )

        TypeTm (TypeForall _) ->
            let
                ( tyvars, ty ) =
                    unForalls [] ty0
            in
            if p < 0 && List.all symbolIsLowercase tyvars then
                viewType model p ty

            else
                ppParen (p >= 0)
                    (row
                        []
                        [ text (String.join " " ("∀" :: List.map symbolToString tyvars) ++ ". ")
                        , viewType model -1 ty
                        ]
                    )

        TypeTm (TypeIntroOuter ty) ->
            viewType model p ty

        TypeAbs _ _ ->
            impossible "viewType: TypeAbs"

        TypeCycle _ ->
            -- impossible?
            text "(not implemented: TypeCycle)"

        TypeTm (TypeAnn _ _) ->
            -- impossible?
            text "(not implemented: TypeAnn)"

        TypeTm (TypeEffect ty1 ty2) ->
            text "(not implemented: TypeEffect)"

        TypeTm (TypeEffects tys) ->
            text "(not implemented: TypeEffects)"


viewTypeRef :
    Model
    -> Reference
    -> Element message
viewTypeRef model reference =
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
                            viewTypeRef2 (HashSet.toList names)


viewTypeRef2 :
    List (List NameSegment)
    -> Element message
viewTypeRef2 names =
    case names of
        [] ->
            impossible "viewTypeRef2: []"

        [ name ] ->
            case listLast name of
                Nothing ->
                    impossible "viewTypeRef2: Nothing"

                Just name2 ->
                    text name2

        names2 ->
            names2
                |> List.map (String.join ".")
                |> String.join "∕"
                |> text


{-| Haskell function: Unison.TypePrinter.arrow
-}
viewArrow :
    Model
    -> Maybe (List (Type Symbol))
    -> Element message
viewArrow model maybeEffects =
    row
        []
        [ text " ->"
        , maybe none (viewEffects model) maybeEffects
        , text " "
        ]


{-| Haskell function: Unison.TypePrinter.arrows
-}
viewArrows :
    Model
    -> List ( Maybe (List (Type Symbol)), Type Symbol )
    -> Element message
viewArrows model input =
    case input of
        [] ->
            none

        ( maybeEffects, ty ) :: tys ->
            row []
                [ viewArrow model maybeEffects
                , viewType model 0 ty
                , viewArrows model tys
                ]


viewEffects :
    Model
    -> List (Type Symbol)
    -> Element message
viewEffects model effects =
    row
        []
        [ text "{"
        , ppCommas (List.map (viewType model 0) effects)
        , text "}"
        ]
