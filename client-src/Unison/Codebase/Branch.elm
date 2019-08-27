module Unison.Codebase.Branch exposing (..)

import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet exposing (HashSet)
import Misc exposing (..)
import Typeclasses.Classes.Equality as Equality
import Typeclasses.Classes.Hashing as Hashing
import Typeclasses.Classes.Monoid exposing (Monoid)
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.NameSegment exposing (..)
import Unison.Hash exposing (..)
import Unison.Reference exposing (..)
import Unison.Referent exposing (..)
import Unison.Util.Relation exposing (..)
import Unison.Util.Star3 exposing (Star3_)
import Util.HashDict as HashDict
import Util.HashSet as HashSet


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

    -- Derived info
    , cache :
        { -- Mapping from type (reference) to a set of its names.
          -- Invariant: the sets are non-empty.
          typeToName : HashDict Reference (HashSet (List NameSegment))
        , typeNames : Relation Reference (List NameSegment)
        }
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
    let
        children : HashDict NameSegment ( BranchHash, Branch )
        children =
            HashDict.foldl
                (\( name, hash ) -> HashDict.insert name ( hash, hashToBranch hash ))
                (HashDict.empty nameSegmentEquality nameSegmentHashing)
                rawBranch.children

        typeNames : Relation Reference (List NameSegment)
        typeNames =
            relationUnion
                (relationMapRange
                    referenceEquality
                    referenceHashing
                    (Equality.list nameSegmentEquality)
                    (Hashing.list nameSegmentHashing)
                    List.singleton
                    rawBranch.types.d1
                )
                (HashDict.foldl
                    (\( name, ( _, Branch child ) ) ->
                        relationUnion
                            (relationMapRange
                                referenceEquality
                                referenceHashing
                                (Equality.list nameSegmentEquality)
                                (Hashing.list nameSegmentHashing)
                                (\names -> name :: names)
                                (rawCausalHead child).cache.typeNames
                            )
                    )
                    (emptyRelation
                        referenceEquality
                        referenceHashing
                        (Equality.list nameSegmentEquality)
                        (Hashing.list nameSegmentHashing)
                    )
                    children
                )

        typeToName : HashDict Reference (HashSet (List NameSegment))
        typeToName =
            HashDict.foldl
                (\( name, ( _, Branch child ) ) ->
                    HashDict.union
                        HashSet.semigroup
                        (HashDict.map
                            referenceEquality
                            referenceHashing
                            (HashSet.map
                                (Equality.list nameSegmentEquality)
                                (Hashing.list nameSegmentHashing)
                                (\names -> name :: names)
                            )
                            (rawCausalHead child).cache.typeToName
                        )
                )
                (HashDict.map
                    referenceEquality
                    referenceHashing
                    (HashSet.map
                        (Equality.list nameSegmentEquality)
                        (Hashing.list nameSegmentHashing)
                        List.singleton
                    )
                    rawBranch.types.d1.domain
                )
                children
    in
    { terms = rawBranch.terms
    , types = rawBranch.types
    , children = children
    , edits = rawBranch.edits
    , cache =
        { typeNames = typeNames
        , typeToName = typeToName
        }
    }
