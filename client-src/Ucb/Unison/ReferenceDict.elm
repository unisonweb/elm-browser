-- | Dict-of-Reference, because passing the Equality and Hashing is annoying.


module Ucb.Unison.ReferenceDict exposing (..)

import HashingContainers.HashDict exposing (HashDict)
import Unison.Reference exposing (..)
import Util.HashDict


type alias ReferenceDict a =
    HashDict Reference a


empty : ReferenceDict a
empty =
    HashingContainers.HashDict.empty referenceEquality referenceHashing


map :
    (a -> b)
    -> ReferenceDict a
    -> ReferenceDict b
map =
    Util.HashDict.map referenceEquality referenceHashing
