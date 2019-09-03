module Ucb.Main.View.Type exposing (viewType)

import Element exposing (..)
import HashingContainers.HashDict as HashDict
import HashingContainers.HashSet as HashSet
import Misc exposing (..)
import Ucb.Main.View.Reference exposing (viewReference)
import Ucb.Unison.Name exposing (..)
import Ucb.Unison.NameDict exposing (NameDict)
import Ucb.Unison.ReferenceSet exposing (ReferenceSet)
import Ucb.Util.Pretty exposing (..)
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.NameSegment exposing (..)
import Unison.Name exposing (..)
import Unison.Reference exposing (..)
import Unison.Symbol exposing (..)
import Unison.Type exposing (..)
import Util.HashSet as HashSet


viewType :
    { r | branch : Branch }
    -> Int
    -> Type Symbol
    -> Element message
viewType view p ty0 =
    case ty0.out of
        TypeVar var ->
            text (symbolToString var)

        TypeTm (TypeRef reference) ->
            viewTypeRef view reference

        TypeTm (TypeArrow ty1 ty2) ->
            ppParen (p >= 0)
                (row
                    []
                    [ viewType view 0 ty1
                    , viewArrows view (typeUnEffectfulArrows ty2)
                    ]
                )

        TypeTm (TypeApp ty1 ty2) ->
            case ty1.out of
                TypeTm (TypeRef (Builtin "Sequence")) ->
                    row
                        []
                        [ text "[", viewType view 0 ty2, text "]" ]

                _ ->
                    case typeUnApps ty0 of
                        Nothing ->
                            impossible "viewType: unApps returned Nothing"

                        Just ( f, xs ) ->
                            ppParen (p >= 10)
                                (row
                                    []
                                    [ viewType view 9 f
                                    , text " "
                                    , ppSpaced (List.map (viewType view 10) xs)
                                    ]
                                )

        TypeTm (TypeForall _) ->
            let
                ( tyvars, ty ) =
                    typeUnForalls [] ty0
            in
            if p < 0 && List.all symbolIsLowercase tyvars then
                viewType view p ty

            else
                ppParen (p >= 0)
                    (row
                        []
                        [ text (String.join " " ("∀" :: List.map symbolToString tyvars) ++ ". ")
                        , viewType view -1 ty
                        ]
                    )

        TypeTm (TypeIntroOuter ty) ->
            viewType view p ty

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
    { r | branch : Branch }
    -> Reference
    -> Element message
viewTypeRef view reference =
    let
        fallback : Element message
        fallback =
            viewReference
                { showBuiltin = True
                , take = Just 7
                }
                reference
    in
    case view.branch of
        Branch causal ->
            let
                head =
                    rawCausalHead causal
            in
            case HashDict.get reference head.cache.typeToName of
                Nothing ->
                    fallback

                Just names ->
                    case HashSet.toList names of
                        [] ->
                            impossible "viewType: empty names"

                        -- TODO, we should handle aliases better. this
                        -- just takes the first name
                        name :: _ ->
                            viewTypeRef2 head.cache.nameToType name


viewTypeRef2 :
    NameDict ReferenceSet
    -> Name
    -> Element message
viewTypeRef2 nameToType fullName =
    text (nameToString (shortenName nameToType fullName))


{-| Haskell function: Unison.TypePrinter.arrow
-}
viewArrow :
    { r | branch : Branch }
    -> Maybe (List (Type Symbol))
    -> Element message
viewArrow view maybeEffects =
    row
        []
        [ text " ->"
        , maybe none (viewEffects view) maybeEffects
        , text " "
        ]


{-| Haskell function: Unison.TypePrinter.arrows
-}
viewArrows :
    { r | branch : Branch }
    -> List ( Maybe (List (Type Symbol)), Type Symbol )
    -> Element message
viewArrows view input =
    case input of
        [] ->
            none

        ( maybeEffects, ty ) :: tys ->
            row []
                [ viewArrow view maybeEffects
                , viewType view 0 ty
                , viewArrows view tys
                ]


viewEffects :
    { r | branch : Branch }
    -> List (Type Symbol)
    -> Element message
viewEffects view effects =
    row
        []
        [ text "{"
        , ppCommas (List.map (viewType view 0) effects)
        , text "}"
        ]
