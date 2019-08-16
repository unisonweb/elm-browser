module Unison.Codebase.Branch exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Unison.Codebase.NameSegment exposing (NameSegment)
import Unison.Hash exposing (Hash)
import Unison.Reference exposing (Reference)
import Unison.Referent exposing (Referent)
import Unison.Util.Relation exposing (Relation)
import Unison.Util.Star3 exposing (Star3)


type alias Type =
    Reference


type alias Value =
    Reference


{-| Haskell type: Unison.Codebase.Branch.Raw
-}
type alias Branch =
    { terms : Star3 () () () () Referent NameSegment Type ( Type, Value )
    , types : Star3 () () () () Reference NameSegment Type ( Type, Value )
    , children : Dict NameSegment Hash
    , edits : Dict NameSegment Hash
    }
