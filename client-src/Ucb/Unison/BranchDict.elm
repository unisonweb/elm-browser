-- | TODO rename to BranchHashDict


module Ucb.Unison.BranchDict exposing (..)

import HashingContainers.HashDict as HashDict exposing (HashDict)
import Typeclasses.Classes.Monoid exposing (Monoid)
import Typeclasses.Classes.Semigroup exposing (Semigroup)
import Unison.Codebase.Branch exposing (..)
import Unison.Hash exposing (..)
import Util.HashDict as HashDict


{-| A dictionary of branchy-things (keyed by branch hash).
-}
type alias BranchDict a =
    HashDict BranchHash a


empty : BranchDict a
empty =
    HashDict.empty hash32Equality hash32Hashing


monoid :
    Semigroup a
    -> Monoid (BranchDict a)
monoid =
    HashDict.monoid hash32Equality hash32Hashing
