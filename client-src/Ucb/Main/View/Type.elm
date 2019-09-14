module Ucb.Main.View.Type exposing (viewType)

import Element exposing (..)
import Element.Events exposing (..)
import Element.Font as Font
import HashingContainers.HashDict as HashDict
import HashingContainers.HashSet as HashSet
import Misc exposing (..)
import Ucb.Main.Message exposing (..)
import Ucb.Main.Model exposing (..)
import Ucb.Main.View.Palette exposing (codeFont, hoverStyle)
import Ucb.Main.View.Reference exposing (viewReference)
import Ucb.Unison.Name exposing (..)
import Ucb.Unison.NameDict exposing (NameDict)
import Ucb.Unison.ReferenceSet exposing (ReferenceSet)
import Ucb.Unison.VType exposing (VType(..), makeVType)
import Ucb.Util.List as List
import Ucb.Util.Pretty exposing (..)
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.NameSegment exposing (..)
import Unison.Name exposing (..)
import Unison.Reference exposing (..)
import Unison.Symbol exposing (..)
import Unison.Type exposing (Type)
import Util.HashSet as HashSet


viewType :
    { r
        | head : Branch
        , hovered : Maybe Hover
        , typeNames : Reference -> List Name
    }
    -- The term this is a type of
    -- TODO what about data constructors :)
    -- TODO whoops, don't want to put a tooltip over all occurrences in a term..
    -> Maybe Reference
    -> Int
    -> VType
    -> Element Message
viewType view termReference p ty0 =
    case ty0 of
        VTypeApp (f :: xs) ->
            ppParen (p >= 10)
                (row
                    []
                    [ viewType view termReference 9 f
                    , text " "
                    , ppSpaced
                        (List.map
                            (viewType view termReference 10)
                            xs
                        )
                    ]
                )

        VTypeArrows ty1 tys ->
            ppParen (p >= 0)
                (row
                    []
                    [ viewType view termReference 0 ty1
                    , viewArrows view termReference tys
                    ]
                )

        VTypeEffects effects ty ->
            ppParen (p >= 10)
                (row
                    []
                    [ viewEffects view termReference effects
                    , text " "
                    , viewType view termReference 10 ty
                    ]
                )

        VTypeForall tyvars ty ->
            if p < 0 && List.all symbolIsLowercase tyvars then
                viewType view termReference p ty

            else
                ppParen (p >= 0)
                    (row
                        []
                        [ text (String.join " " ("∀" :: List.map symbolToString tyvars) ++ ". ")
                        , viewType view termReference -1 ty
                        ]
                    )

        VTypeRef reference ->
            viewTypeRef view termReference reference

        VTypeSequence ty ->
            row
                []
                [ text "["
                , viewType view termReference 0 ty
                , text "]"
                ]

        VTypeVar var ->
            text var

        VType___UNKNOWN ty ->
            text (Debug.toString ty)

        VTypeApp [] ->
            impossible "VTypeApp []"


viewTypeRef :
    { r
        | head : Branch
        , hovered : Maybe Hover
        , typeNames : Reference -> List Name
    }
    -> Maybe Reference
    -> Reference
    -> Element Message
viewTypeRef view maybeTermReference reference =
    case view.typeNames reference of
        -- Weird.. don't think this should happen
        -- Or maybe it totally could if you're mid-refactor?
        [] ->
            viewReference
                { showBuiltin = True
                , take = Just 7
                }
                reference

        -- TODO, we should handle aliases better. this just displays the first
        -- name
        name :: names ->
            let
                shortName : Name
                shortName =
                    shortenName (branchHead view.head).cache.nameToType name
            in
            el
                (case maybeTermReference of
                    Nothing ->
                        []

                    Just termReference ->
                        [ above <|
                            if view.hovered == Just (HoverType termReference reference) then
                                el
                                    hoverStyle
                                    (column
                                        []
                                        ((case reference of
                                            Builtin _ ->
                                                identity

                                            Derived { hash } ->
                                                List.cons
                                                    (el
                                                        [ Font.color (rgb 0.5 0.5 0.5) ]
                                                        (text hash)
                                                    )
                                         )
                                            (List.map
                                                (nameToString >> text)
                                                (name :: names)
                                            )
                                        )
                                    )

                            else
                                none
                        , codeFont
                        , onMouseEnter (User_Hover (HoverType termReference reference))
                        , onMouseLeave User_Unhover
                        ]
                )
                (text (nameToString shortName))


{-| Haskell function: Unison.TypePrinter.arrow
-}
viewArrow :
    { r
        | head : Branch
        , hovered : Maybe Hover
        , typeNames : Reference -> List Name
    }
    -> Maybe Reference
    -> List VType
    -> Element Message
viewArrow view termReference effects =
    row
        []
        [ text " ->"
        , viewEffects view termReference effects
        , text " "
        ]


{-| Haskell function: Unison.TypePrinter.arrows
-}
viewArrows :
    { r
        | head : Branch
        , hovered : Maybe Hover
        , typeNames : Reference -> List Name
    }
    -> Maybe Reference
    -> List ( List VType, VType )
    -> Element Message
viewArrows view termReference input =
    case input of
        [] ->
            none

        ( effects, ty ) :: tys ->
            row []
                [ viewArrow view termReference effects
                , viewType view termReference 0 ty
                , viewArrows view termReference tys
                ]


viewEffects :
    { r
        | head : Branch
        , hovered : Maybe Hover
        , typeNames : Reference -> List Name
    }
    -> Maybe Reference
    -> List VType
    -> Element Message
viewEffects view termReference effects =
    if List.isEmpty effects then
        none

    else
        row
            []
            [ text "{"
            , ppCommas (List.map (viewType view termReference 0) effects)
            , text "}"
            ]
