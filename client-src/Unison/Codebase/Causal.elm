module Unison.Codebase.Causal exposing (..)

import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Unison.Codebase.NameSegment exposing (..)
import Unison.Hash exposing (..)


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


rawCausalChildren :
    RawCausal { r | children : HashDict NameSegment a }
    -> List a
rawCausalChildren =
    rawCausalHead
        >> .children
        >> HashDict.toList
        >> List.map Tuple.second


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
