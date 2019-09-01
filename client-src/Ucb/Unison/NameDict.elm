-- | Dict-of-Name, because passing the Equality and Hashing is annoying.


module Ucb.Unison.NameDict exposing (..)

import HashingContainers.HashDict as HashDict exposing (HashDict)
import Unison.Name exposing (..)
import Util.HashDict as HashDict


type alias NameDict a =
    HashDict Name a


empty : NameDict a
empty =
    HashDict.empty nameEquality nameHashing


mapKeys :
    (k -> Name)
    -> HashDict k a
    -> NameDict a
mapKeys =
    HashDict.mapKeys nameEquality nameHashing
