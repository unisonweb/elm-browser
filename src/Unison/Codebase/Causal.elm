module Unison.Codebase.Causal exposing (..)

import Set exposing (Set)
import Unison.Codebase.Branch exposing (..)
import Unison.Hash exposing (Hash)


{-| Haskell type: Unison.Codebase.Causal.Raw
-}
type Causal
    = RawOne Branch
    | RawCons Branch Hash
    | RawMerge Branch (Set Hash)
