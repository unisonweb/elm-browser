module Unison.Parser exposing (..)

import Unison.Lexer exposing (Pos)


{-| Haskell type: Unison.Parser.Ann
-}
type Ann
    = Intrinsic
    | External
    | Ann { start : Pos, end : Pos }
