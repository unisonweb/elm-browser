module Unison.Type exposing (..)

import HashingContainers.HashSet exposing (HashSet)
import Unison.ABT exposing (..)
import Unison.Kind exposing (Kind)
import Unison.Reference exposing (Reference)


{-| Haskell type: Unison.Type.Type
-}
type alias Type var ann =
    AbtTerm var ann (TypeTerm var ann)


type TypeTerm var ann
    = TypeVar var
    | TypeCycle (Type var ann)
    | TypeAbs var (Type var ann)
    | TypeRef Reference
    | TypeArrow (Type var ann) (Type var ann)
    | TypeAnn (Type var ann) Kind
    | TypeApp (Type var ann) (Type var ann)
    | TypeEffect (Type var ann) (Type var ann)
    | TypeEffects (List (Type var ann))
    | TypeForall (Type var ann)
    | TypeIntroOuter (Type var ann)
