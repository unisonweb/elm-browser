module Unison.Codebase.Branch exposing (..)

import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet exposing (HashSet)
import Misc exposing (impossible)
import Unison.Codebase.Causal exposing (RawCausal)
import Unison.Codebase.NameSegment exposing (..)
import Unison.Hash exposing (Hash32)
import Unison.Reference exposing (..)
import Unison.Referent exposing (..)
import Unison.Util.Relation exposing (Relation)
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
    , children : HashDict NameSegment Branch
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


type alias BranchHash =
    Hash32


{-| Make a 'Branch0' from a 'RawBranch', given a map of branches.
Invariant: our descendants are all in the given map.
-}
rawBranchToBranch0 :
    HashDict BranchHash Branch
    -> RawBranch
    -> Branch0
rawBranchToBranch0 branches rawBranch =
    { terms = rawBranch.terms
    , types = rawBranch.types
    , children =
        HashDict.foldl
            (\( nameSegment, hash ) ->
                HashDict.insert
                    nameSegment
                    (case HashDict.get hash branches of
                        Nothing ->
                            impossible "makeBranch0: Nothing"

                        Just branch ->
                            branch
                    )
            )
            (HashDict.empty nameSegmentEquality nameSegmentHashing)
            rawBranch.children
    , edits = rawBranch.edits
    }
