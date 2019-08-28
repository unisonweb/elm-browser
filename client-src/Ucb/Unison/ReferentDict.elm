-- | Dict-of-Referent, because passing the Equality and Hashing is annoying.


module Ucb.Unison.ReferentDict exposing (..)

import HashingContainers.HashDict exposing (HashDict)
import Unison.Referent exposing (..)
import Util.HashDict


type alias ReferentDict a =
    HashDict Referent a


empty : ReferentDict a
empty =
    HashingContainers.HashDict.empty referentEquality referentHashing


map :
    (a -> b)
    -> ReferentDict a
    -> ReferentDict b
map =
    Util.HashDict.map referentEquality referentHashing
