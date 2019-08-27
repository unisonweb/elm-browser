--| Pretty-printing toolkit


module Ucb.Util.Pretty exposing (..)

import Element exposing (..)


{-| Haskell function: Unison.Util.Pretty.commas
-}
ppCommas : List (Element message) -> Element message
ppCommas =
    List.intersperse (text ", ") >> row []


{-| Haskell function: Unison.TypePrinter.paren
-}
ppParen : Bool -> Element message -> Element message
ppParen b x =
    if b then
        row [] [ text "(", x, text ")" ]

    else
        x


{-| Haskell function: Unison.Util.Pretty.spaced
-}
ppSpaced : List (Element message) -> Element message
ppSpaced =
    List.intersperse (text " ") >> row []
