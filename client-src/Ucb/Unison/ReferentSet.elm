-- | Set-of-Referent, because passing the Equality and Hashing is annoying.


module Ucb.Unison.ReferentSet exposing (..)

import HashingContainers.HashSet as HashSet exposing (HashSet)
import Unison.Referent exposing (..)
import Util.HashSet as HashSet


type alias ReferentSet =
    HashSet Referent


empty : ReferentSet
empty =
    HashSet.empty referentEquality referentHashing


map :
    (Referent -> Referent)
    -> ReferentSet
    -> ReferentSet
map =
    HashSet.map referentEquality referentHashing


singleton : Referent -> ReferentSet
singleton =
    HashSet.singleton referentEquality referentHashing
