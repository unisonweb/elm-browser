module Unison.Codebase.Branch exposing (..)

import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet exposing (HashSet)
import Misc exposing (..)
import Typeclasses.Classes.Monoid exposing (Monoid)
import Unison.Codebase.Causal exposing (RawCausal)
import Unison.Codebase.NameSegment exposing (..)
import Unison.Hash exposing (..)
import Unison.Reference exposing (..)
import Unison.Referent exposing (..)
import Unison.Util.Star3 exposing (Star3_)


type alias Star a n =
    Star3_ a n Reference ( Reference, Reference )


{-| Haskell type: Unison.Codebase.Branch.Branch, kind of. Because we can't have
monadic actions tucked inside a data structure, we still have a RawCausal full
of hashes. But _its_ head is a Branch0, not a RawBranch. So it's a tree of types
and terms, without any pre-fetched history.
-}
type Branch
    = Branch (RawCausal Branch0)


{-| Haskell type: Unison.Codebase.Branch.Branch0, kind of. See above.
-}
type alias Branch0 =
    { terms : Star Referent NameSegment
    , types : Star Reference NameSegment
    , children : HashDict NameSegment ( BranchHash, Branch )
    , edits : HashDict NameSegment Hash32 -- TODO

    -- TODO deepTerms, deepTypes, etc
    }


{-| Haskell type: Unison.Codebase.Branch.Raw
-}
type alias RawBranch =
    { terms : Star Referent NameSegment
    , types : Star Reference NameSegment
    , children : HashDict NameSegment BranchHash
    , edits : HashDict NameSegment BranchHash
    }


{-| The hash of a branch.
-}
type alias BranchHash =
    Hash32


{-| Make a 'Branch0' from a 'RawBranch'.
-}
rawBranchToBranch0 :
    (BranchHash -> Branch)
    -> RawBranch
    -> Branch0
rawBranchToBranch0 hashToBranch rawBranch =
    { terms = rawBranch.terms
    , types = rawBranch.types
    , children =
        HashDict.foldl
            (\( name, hash ) -> HashDict.insert name ( hash, hashToBranch hash ))
            (HashDict.empty nameSegmentEquality nameSegmentHashing)
            rawBranch.children
    , edits = rawBranch.edits
    }
