module Unison.ABT exposing (..)

import HashingContainers.HashSet exposing (HashSet)


type alias AbtTerm var term =
    { freeVars : HashSet var
    , out : term
    }
