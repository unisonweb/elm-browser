-- | Dict-of-Name, because passing the Equality and Hashing is annoying.


module Ucb.Unison.NameDict exposing (..)

import HashingContainers.HashDict as HashDict exposing (HashDict)
import Unison.Name exposing (..)


type alias NameDict a =
    HashDict Name a


empty : NameDict a
empty =
    HashDict.empty nameEquality nameHashing
