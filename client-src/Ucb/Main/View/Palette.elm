module Ucb.Main.View.Palette exposing (..)

import Element exposing (Attribute, rgb)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font



{- TODO:
   We really should not be using external
   because it causes font flash.
   Once we have a #realBuild w/ webpack get rid of `external`
-}


mainFont : Attribute message
mainFont =
    Font.family
        [ Font.external
            { url = "https://fonts.googleapis.com/css?family=Manjari&display=swap"
            , name = "Manjari"
            }
        , Font.typeface "sans-serif"
        ]


codeFont : Attribute message
codeFont =
    Font.family
        [ Font.external
            { url = "https://fonts.googleapis.com/css?family=Roboto+Mono&display=swap"
            , name = "Roboto Mono"
            }
        , Font.typeface "monospace"
        ]


{-| Styling for hovered types/terms
-}
hoverStyle : List (Attribute message)
hoverStyle =
    [ Background.color (rgb 1 1 1)
    , Border.color (rgb 0 0 0)
    , Border.solid
    , Border.width 1
    ]
