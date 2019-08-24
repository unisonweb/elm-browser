module Unison.Codebase.Causal exposing (..)

import HashingContainers.HashSet as HashSet exposing (HashSet)
import Unison.Hash exposing (Hash32)


{-| Haskell type: Unison.Codebase.Causal.Raw
-}
type RawCausal branch
    = RawOne branch
    | RawCons branch Hash32
    | RawMerge branch (HashSet Hash32)


rawCausalHead :
    RawCausal branch
    -> branch
rawCausalHead causal =
    case causal of
        RawOne branch ->
            branch

        RawCons branch _ ->
            branch

        RawMerge branch _ ->
            branch


{-| Predecessors in the causal chain.
-}
rawCausalPredecessors :
    RawCausal branch
    -> List Hash32
rawCausalPredecessors causal =
    case causal of
        RawOne _ ->
            []

        RawCons _ hash ->
            [ hash ]

        RawMerge _ hashes ->
            HashSet.toList hashes


rawCausalMap :
    (a -> b)
    -> RawCausal a
    -> RawCausal b
rawCausalMap f causal =
    case causal of
        RawOne x ->
            RawOne (f x)

        RawCons x y ->
            RawCons (f x) y

        RawMerge x y ->
            RawMerge (f x) y
