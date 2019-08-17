module Unison.Symbol exposing (..)

import Unison.Var exposing (VarType)
import Word64 exposing (Word64)


{-| Unison type: Unison.Symbol.Symbol
-}
type Symbol
    = Symbol Word64 VarType
