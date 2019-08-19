module Unison.Type exposing (..)

import HashingContainers.HashSet as HashSet exposing (HashSet)
import Misc exposing (..)
import Typeclasses.Classes.Equality exposing (Equality)
import Typeclasses.Classes.Hashing exposing (Hashing)
import Unison.ABT exposing (..)
import Unison.Kind exposing (Kind)
import Unison.Reference exposing (Reference)


{-| Haskell type: Unison.Type.Type
-}
type alias Type var =
    AbtTerm var (TypeAbt var)


type TypeAbt var
    = TypeVar var
    | TypeCycle (Type var)
    | TypeAbs var (Type var)
    | TypeTm (TypeF var)


type TypeF var
    = TypeRef Reference
    | TypeArrow (Type var) (Type var)
    | TypeAnn (Type var) Kind
    | TypeApp (Type var) (Type var)
    | TypeEffect (Type var) (Type var)
    | TypeEffects (List (Type var))
    | TypeForall (Type var)
    | TypeIntroOuter (Type var)


typeFFreeVars :
    Equality var
    -> Hashing var
    -> TypeF var
    -> HashSet var
typeFFreeVars varEquality varHashing term =
    case term of
        TypeRef _ ->
            HashSet.empty varEquality varHashing

        TypeArrow t1 t2 ->
            hashSetUnion t1.freeVars t2.freeVars

        TypeAnn t _ ->
            t.freeVars

        TypeApp t1 t2 ->
            hashSetUnion t1.freeVars t2.freeVars

        TypeEffect t1 t2 ->
            hashSetUnion t1.freeVars t2.freeVars

        TypeEffects ts ->
            hashSetUnions varEquality varHashing (List.map .freeVars ts)

        TypeForall t ->
            t.freeVars

        TypeIntroOuter t ->
            t.freeVars


typeVar :
    Equality var
    -> Hashing var
    -> var
    -> Type var
typeVar varEquality varHashing =
    abtVar varEquality varHashing TypeVar


typeAbs :
    var
    -> Type var
    -> Type var
typeAbs =
    abtAbs TypeAbs


typeCycle :
    Type var
    -> Type var
typeCycle =
    abtCycle TypeCycle


typeTerm :
    Equality var
    -> Hashing var
    -> TypeF var
    -> Type var
typeTerm varEquality varHashing ty =
    { freeVars = typeFFreeVars varEquality varHashing ty
    , out = TypeTm ty
    }
