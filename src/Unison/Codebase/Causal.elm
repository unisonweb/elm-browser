module Unison.Codebase.Causal exposing (..)

import Set exposing (Set)
import Unison.Codebase.Branch exposing (..)
import Unison.Hash exposing (Hash)


{-| Haskell type: Unison.Codebase.Causal.Raw
-}
type RawCausal
    = RawOne RawBranch
    | RawCons RawBranch Hash
    | RawMerge RawBranch (Set Hash)
