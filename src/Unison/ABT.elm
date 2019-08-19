module Unison.ABT exposing (..)

import HashingContainers.HashSet exposing (HashSet)


type alias AbtTerm var ann term =
    { freeVars : HashSet var
    , annotation : ann
    , out : term
    }
