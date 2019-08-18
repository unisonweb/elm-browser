module Unison.Codebase.Branch exposing (..)

import HashingContainers.HashDict exposing (HashDict)
import HashingContainers.HashSet exposing (HashSet)
import Unison.Codebase.NameSegment exposing (NameSegment)
import Unison.Hash exposing (Hash32)
import Unison.Reference exposing (..)
import Unison.Referent exposing (..)
import Unison.Util.Relation exposing (Relation)
import Unison.Util.Star3 exposing (Star3_)


type alias Type =
    Reference


type alias Value =
    Reference


type alias Star a n =
    Star3_ a n Type ( Type, Value )


{-| Haskell type: Unison.Codebase.Branch.Raw
-}
type alias RawBranch =
    { terms : Star Referent NameSegment
    , types : Star Reference NameSegment
    , children : HashDict NameSegment Hash32
    , edits : HashDict NameSegment Hash32
    }


{-| A flattened out RawBranch. Mitchell is not really sure if it would be nicer
to work with for the UI, but wanted to see all the bits and pieces of a branch
inlined, so here it is.
-}
type alias RawBranch2 =
    { terms : HashSet Referent
    , termsNames : Relation Referent NameSegment
    , termsTypes : Relation Referent Type
    , termsThings : Relation Referent ( Type, Value ) -- what are these?
    , types : HashSet Reference
    , typesNames : Relation Reference NameSegment
    , typesTypes : Relation Reference Type
    , typesThings : Relation Reference ( Type, Value ) -- what are these?
    , children : HashDict NameSegment Hash32
    , edits : HashDict NameSegment Hash32
    }
