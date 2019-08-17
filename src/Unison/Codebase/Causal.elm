module Unison.Codebase.Causal exposing (..)

import HashingContainers.HashSet exposing (HashSet)
import Unison.Codebase.Branch exposing (..)
import Unison.Hash exposing (Hash32)


{-| Haskell type: Unison.Codebase.Causal.Raw
-}
type RawCausal
    = RawOne RawBranch
    | RawCons RawBranch Hash32
    | RawMerge RawBranch (HashSet Hash32)
