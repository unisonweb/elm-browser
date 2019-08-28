-- | Set-of-Reference, because passing the Equality and Hashing is annoying.


module Ucb.Unison.ReferenceSet exposing (..)

import HashingContainers.HashSet as HashSet exposing (HashSet)
import Unison.Reference exposing (..)
import Util.HashSet as HashSet


type alias ReferenceSet =
    HashSet Reference


empty : ReferenceSet
empty =
    HashSet.empty referenceEquality referenceHashing


map :
    (Reference -> Reference)
    -> ReferenceSet
    -> ReferenceSet
map =
    HashSet.map referenceEquality referenceHashing


singleton : Reference -> ReferenceSet
singleton =
    HashSet.singleton referenceEquality referenceHashing
