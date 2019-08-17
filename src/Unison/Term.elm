module Unison.Term exposing (..)

import Int64 exposing (Int64)
import Unison.Blank exposing (Blank)
import Unison.Reference exposing (Reference)
import Word64 exposing (Word64)


{-| Haskell type: Unison.Term.Term
-}
type Term v a
    = Undefined
