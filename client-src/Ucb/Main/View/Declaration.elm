module Ucb.Main.View.Declaration exposing (viewDeclaration)

import Element exposing (..)
import Ucb.Main.View.Symbol exposing (..)
import Unison.ConstructorType exposing (..)
import Unison.Declaration exposing (..)
import Unison.Symbol exposing (..)


viewDeclaration :
    Declaration Symbol
    -> Element message
viewDeclaration declaration =
    case declaration of
        DataDecl dataDeclaration ->
            viewDataDeclaration Data dataDeclaration

        EffectDecl dataDeclaration ->
            viewDataDeclaration Effect dataDeclaration


viewDataDeclaration :
    ConstructorType
    -> DataDeclaration Symbol
    -> Element message
viewDataDeclaration constructorType declaration =
    column
        []
        [ case constructorType of
            Data ->
                case declaration.modifier of
                    Structural ->
                        text "structural type"

                    Unique _ ->
                        text "unique type"

            Effect ->
                text "ability"
        , case declaration.bound of
            [] ->
                none

            bound ->
                row
                    [ spacing 5 ]
                    (text "bound" :: List.map viewSymbol declaration.bound)
        , case declaration.constructors of
            [] ->
                none

            constructors ->
                row
                    [ spacing 5 ]
                    (text "constructors"
                        :: List.map
                            (\( name, type_ ) ->
                                row [ spacing 2 ] [ viewSymbol name ]
                            )
                            declaration.constructors
                    )
        ]
