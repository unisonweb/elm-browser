module Unison.Codebase.Branch exposing
    ( Branch(..)
    , Branch0
    , BranchHash
    , RawBranch
    , Star
    , branchPatchHashes
    , branchPatches
    , rawBranchToBranch0
    )

import Array
import HashingContainers.HashDict as HashDict
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Misc exposing (..)
import Ucb.Unison.NameDict as NameDict exposing (NameDict)
import Ucb.Unison.NameSegmentDict as NameSegmentDict exposing (NameSegmentDict)
import Ucb.Unison.NameSet as NameSet exposing (NameSet)
import Ucb.Unison.ReferenceDict as ReferenceDict exposing (ReferenceDict)
import Ucb.Unison.ReferenceSet as ReferenceSet exposing (ReferenceSet)
import Ucb.Unison.ReferentDict as ReferentDict exposing (ReferentDict)
import Ucb.Unison.ReferentSet as ReferentSet exposing (ReferentSet)
import Ucb.Util.Array as Array
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.NameSegment exposing (..)
import Unison.Codebase.Patch exposing (..)
import Unison.Hash exposing (..)
import Unison.Name as Name exposing (..)
import Unison.Reference exposing (..)
import Unison.Referent exposing (..)
import Unison.Util.Relation exposing (..)
import Unison.Util.Star3 exposing (Star3_)
import Util.HashDict as HashDict
import Util.HashSet as HashSet



-- TODO are all Array.push in this module meant to be Array.cons?


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
    , children : NameSegmentDict ( BranchHash, Branch )
    , patches : NameSegmentDict PatchHash

    -- Derived info
    , cache :
        { -- Mapping from name to child branch
          pathToChild : NameDict ( BranchHash, Branch )

        -- Mapping from term (referent) to a set of its names.
        -- Invariant: the sets are non-empty.
        , termToName : ReferentDict NameSet

        -- Inverted mapping, but including name suffixes. See comment on
        -- 'makeNameToTerm' below.
        , nameToTerm : NameDict ReferentSet

        -- Same as termToName/nameToTerm, but for types
        , typeToName : ReferenceDict NameSet
        , nameToType : NameDict ReferenceSet
        }
    }


{-| Haskell type: Unison.Codebase.Branch.Raw
-}
type alias RawBranch =
    { terms : Star Referent NameSegment
    , types : Star Reference NameSegment
    , children : NameSegmentDict BranchHash
    , patches : NameSegmentDict PatchHash
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
        children : NameSegmentDict ( BranchHash, Branch )
        children =
            HashDict.foldl
                (\( name, hash ) -> HashDict.insert name ( hash, hashToBranch hash ))
                NameSegmentDict.empty
                rawBranch.children

        pathToChild : NameDict ( BranchHash, Branch )
        pathToChild =
            HashDict.foldl
                (\( name, ( hash, Branch child ) ) acc ->
                    HashDict.insert
                        (nameFromNameSegment name)
                        ( hash, Branch child )
                        (HashDict.foldl
                            (\( path, grandchild ) ->
                                HashDict.insert (nameCons name path) grandchild
                            )
                            acc
                            (rawCausalHead child).cache.pathToChild
                        )
                )
                NameDict.empty
                children

        termToName : ReferentDict NameSet
        termToName =
            makeTermToName
                rawBranch.terms.d1.domain
                children

        typeToName : ReferenceDict NameSet
        typeToName =
            makeTypeToName
                rawBranch.types.d1.domain
                children
    in
    { terms = rawBranch.terms
    , types = rawBranch.types
    , children = children
    , patches = rawBranch.patches
    , cache =
        { pathToChild = pathToChild
        , termToName = termToName
        , nameToTerm = makeNameToTerm termToName
        , typeToName = typeToName
        , nameToType = makeNameToType typeToName
        }
    }


{-| Given the current branch's referent names

      #foobar -> { "abc", "xyz" }  // this referent has two aliases
      #bazqux -> { "123" }

    and the children branches (whose termToNames we are interested in)

      "child1" -> ( #aaa -> { ["a", "b", "c"] }
                    #bbb -> { ["ddd"] )

      "child2" -> ( #ccc -> { ["ccc"] } )

    combine them together to one master mapping, that simply cons's each child's
    name onto *their* childrens' names, and turns the current branch's name
    segments into single-segment names.

      #foobar -> { ["abc"], ["xyz"] }
      #bazqux -> { ["123"] }
      #aaa    -> { ["child1", "a", "b", "d"] }
      #bbb    -> { ["child1", "ddd"] }
      #ccc    -> { ["child2", "ccc"] }

-}
makeTermToName :
    ReferentDict (HashSet NameSegment)
    -> NameSegmentDict ( BranchHash, Branch )
    -> ReferentDict NameSet
makeTermToName branch =
    HashDict.foldl
        (\( name, ( _, Branch child ) ) ->
            HashDict.union
                HashSet.semigroup
                (ReferentDict.map
                    (NameSet.map (nameCons name))
                    (rawCausalHead child).cache.termToName
                )
        )
        (ReferentDict.map
            (HashSet.map nameEquality nameHashing nameFromNameSegment)
            branch
        )


{-| Like the reverse mapping of termToName, except that when inverting a mapping
like

      #aaa -> { ["a", "b", "c"] }

    we don't just store

      ["a", "b", "c"] -> { #aaa }

    but also

      ["b", "c"] -> { #aaa }
      ["c"]      -> { #aaa }

    This "suffix-name" lookup table allows us to compute the shortest possible
    unambiguous name for a term in a branch with the following simple algorithm:

    * First, look up the forward mapping from referent to (full) name.

    * Then, for each suffix of the name (beginning with the shortest), look the
      referents up in the reversed mapping, until we find a singleton set.

    * If we don't even find a singleton set all the way up to the full name of
      the referent, that just means we have the same name for two referents
      (due to a merge, for example).

-}
makeNameToTerm :
    ReferentDict NameSet
    -> NameDict ReferentSet
makeNameToTerm =
    let
        f1 :
            ( Referent, NameSet )
            -> NameDict ReferentSet
            -> NameDict ReferentSet
        f1 =
            f2 >> HashDict.union HashSet.semigroup

        f2 :
            ( Referent, NameSet )
            -> NameDict ReferentSet
        f2 ( referent, names ) =
            HashSet.foldl
                (f3 referent)
                NameDict.empty
                names

        f3 :
            Referent
            -> Name
            -> NameDict ReferentSet
            -> NameDict ReferentSet
        f3 referent name acc =
            List.foldl
                (\suffix ->
                    HashDict.insert
                        suffix
                        (ReferentSet.singleton referent)
                )
                acc
                (nameTails name)
    in
    HashDict.foldl f1 NameDict.empty


{-| See 'makeTermToName'
-}
makeTypeToName :
    ReferenceDict (HashSet NameSegment)
    -> NameSegmentDict ( BranchHash, Branch )
    -> ReferenceDict NameSet
makeTypeToName branch =
    HashDict.foldl
        (\( name, ( _, Branch child ) ) ->
            HashDict.union
                HashSet.semigroup
                (ReferenceDict.map
                    (NameSet.map (nameCons name))
                    (rawCausalHead child).cache.typeToName
                )
        )
        (ReferenceDict.map
            (HashSet.map nameEquality nameHashing nameFromNameSegment)
            branch
        )


{-| See 'makeNameToTerm'
-}
makeNameToType :
    ReferenceDict NameSet
    -> NameDict ReferenceSet
makeNameToType =
    let
        f1 :
            ( Reference, NameSet )
            -> NameDict ReferenceSet
            -> NameDict ReferenceSet
        f1 =
            f2 >> HashDict.union HashSet.semigroup

        f2 :
            ( Reference, NameSet )
            -> NameDict ReferenceSet
        f2 ( reference, names ) =
            HashSet.foldl
                (f3 reference)
                NameDict.empty
                names

        f3 :
            Reference
            -> Name
            -> NameDict ReferenceSet
            -> NameDict ReferenceSet
        f3 reference name acc =
            List.foldl
                (\suffix ->
                    HashDict.insert
                        suffix
                        (ReferenceSet.singleton reference)
                )
                acc
                (nameTails name)
    in
    HashDict.foldl f1 NameDict.empty


{-| Compute the full names of all the patches in a Branch.

    Conceptually it's very simple. A branch has top-level patches, and its
    children have patches. Say our patches are:

        patch = Patch1

    and we have one child, "foo", whose patches are:

        patch = Patch2
        blah  = Patch3

    then, this function computes:

        patch     = Patch1
        foo.patch = Patch2
        foo.blah  = Patch3

-}
branchPatches :
    Branch
    -> NameDict PatchHash
branchPatches (Branch causal) =
    branchPatches0 (rawCausalHead causal)


{-| Compute the full names of all the patches in a Branch0.
-}
branchPatches0 :
    Branch0
    -> NameDict PatchHash
branchPatches0 branch =
    let
        initial : NameDict PatchHash
        initial =
            NameDict.mapKeys
                nameFromNameSegment
                branch.patches

        step :
            ( NameSegment, ( BranchHash, Branch ) )
            -> NameDict PatchHash
            -> NameDict PatchHash
        step ( name, ( _, child ) ) acc =
            HashDict.foldl
                (\( suffix, hash ) ->
                    HashDict.insert
                        (nameCons name suffix)
                        hash
                )
                acc
                (branchPatches child)
    in
    HashDict.foldl
        step
        initial
        branch.children


{-| Like 'branchPatches', but throw away the names and just return the hashes.
-}
branchPatchHashes :
    Branch
    -> HashSet PatchHash
branchPatchHashes =
    branchPatches
        >> HashDict.toList
        >> List.map Tuple.second
        >> HashSet.fromList hash32Equality hash32Hashing
