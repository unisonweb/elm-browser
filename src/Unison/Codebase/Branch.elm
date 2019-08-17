module Unison.Codebase.Branch exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Unison.Codebase.NameSegment exposing (NameSegment)
import Unison.Hash exposing (Hash)
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
    , children : Dict NameSegment Hash
    , edits : Dict NameSegment Hash
    }
