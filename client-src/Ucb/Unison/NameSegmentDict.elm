-- | Dict-of-NameSegment, because passing the Equality and Hashing is annoying.


module Ucb.Unison.NameSegmentDict exposing (..)

import HashingContainers.HashDict exposing (HashDict)
import Unison.Codebase.NameSegment exposing (..)


type alias NameSegmentDict a =
    HashDict NameSegment a


empty : NameSegmentDict a
empty =
    HashingContainers.HashDict.empty nameSegmentEquality nameSegmentHashing
