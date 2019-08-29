-- | Set-of-Name, because passing the Equality and Hashing is annoying.


module Ucb.Unison.NameSet exposing (..)

import HashingContainers.HashSet as HashSet exposing (HashSet)
import Unison.Name exposing (..)
import Util.HashSet as HashSet


type alias NameSet =
    HashSet Name


empty : NameSet
empty =
    HashSet.empty nameEquality nameHashing


fromList : List Name -> NameSet
fromList =
    HashSet.fromList nameEquality nameHashing


map :
    (Name -> Name)
    -> NameSet
    -> NameSet
map =
    HashSet.map nameEquality nameHashing
