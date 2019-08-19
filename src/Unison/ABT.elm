module Unison.ABT exposing (..)

import HashingContainers.HashSet as HashSet exposing (HashSet)
import Misc exposing (hashSetSingleton)
import Typeclasses.Classes.Equality exposing (Equality)
import Typeclasses.Classes.Hashing exposing (Hashing)


type alias AbtTerm var term =
    { freeVars : HashSet var
    , out : term
    }


abtVar :
    Equality var
    -> Hashing var
    -> (var -> term)
    -> var
    -> AbtTerm var term
abtVar equality hashing tmVar var =
    { freeVars = hashSetSingleton equality hashing var
    , out = tmVar var
    }


abtAbs :
    (var -> AbtTerm var term -> term)
    -> var
    -> AbtTerm var term
    -> AbtTerm var term
abtAbs tmAbs var body =
    { freeVars = HashSet.remove var body.freeVars
    , out = tmAbs var body
    }


abtCycle :
    (AbtTerm var term -> term)
    -> AbtTerm var term
    -> AbtTerm var term
abtCycle tmCycle term =
    { freeVars = term.freeVars
    , out = tmCycle term
    }
