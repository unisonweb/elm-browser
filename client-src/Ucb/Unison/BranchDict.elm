module Ucb.Unison.BranchDict exposing (..)

import HashingContainers.HashDict as HashDict exposing (HashDict)
import Misc exposing (..)
import Typeclasses.Classes.Monoid exposing (Monoid)
import Typeclasses.Classes.Semigroup exposing (Semigroup)
import Unison.Codebase.Branch exposing (..)
import Unison.Hash exposing (..)


{-| A dictionary of branchy-things (keyed by branch hash).
-}
type alias BranchDict a =
    HashDict BranchHash a


emptyBranchDict : BranchDict a
emptyBranchDict =
    HashDict.empty hash32Equality hash32Hashing


branchDictMonoid :
    Semigroup a
    -> Monoid (BranchDict a)
branchDictMonoid =
    hashDictMonoid hash32Equality hash32Hashing
