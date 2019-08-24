module Ucb.Main.View.Symbol exposing (viewSymbol)

import Element exposing (..)
import Unison.Symbol exposing (..)
import Unison.Var exposing (..)
import Word64 exposing (..)


viewSymbol :
    Symbol
    -> Element message
viewSymbol symbol =
    case symbol of
        Symbol n var ->
            case var of
                User name ->
                    let
                        m : Int
                        m =
                            unsafeWord64ToWord53 n
                    in
                    text
                        (name
                            ++ (if m == 0 then
                                    ""

                                else
                                    String.fromInt m
                               )
                        )

                _ ->
                    text "???"
