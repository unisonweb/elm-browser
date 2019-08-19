module Unison.Type exposing (..)

import HashingContainers.HashSet exposing (HashSet)
import Unison.ABT exposing (..)
import Unison.Kind exposing (Kind)
import Unison.Reference exposing (Reference)


{-| Haskell type: Unison.Type.Type
-}
type alias Type var =
    AbtTerm var (TypeTerm var)


type TypeTerm var
    = TypeVar var
    | TypeCycle (Type var)
    | TypeAbs var (Type var)
    | TypeRef Reference
    | TypeArrow (Type var) (Type var)
    | TypeAnn (Type var) Kind
    | TypeApp (Type var) (Type var)
    | TypeEffect (Type var) (Type var)
    | TypeEffects (List (Type var))
    | TypeForall (Type var)
    | TypeIntroOuter (Type var)
