module Unison.Type exposing (..)

import HashingContainers.HashSet as HashSet exposing (HashSet)
import Misc exposing (..)
import Typeclasses.Classes.Equality exposing (Equality)
import Typeclasses.Classes.Hashing exposing (Hashing)
import Unison.ABT exposing (..)
import Unison.Kind exposing (Kind)
import Unison.Reference exposing (..)
import Util.HashSet as HashSet


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
            HashSet.union t1.freeVars t2.freeVars

        TypeAnn t _ ->
            t.freeVars

        TypeApp t1 t2 ->
            HashSet.union t1.freeVars t2.freeVars

        TypeEffect t1 t2 ->
            HashSet.union t1.freeVars t2.freeVars

        TypeEffects ts ->
            HashSet.unions varEquality varHashing (List.map .freeVars ts)

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


{-| Return a set of references inside a type.
-}
typeReferences :
    Type var
    -> HashSet Reference
typeReferences { out } =
    case out of
        TypeVar _ ->
            HashSet.empty referenceEquality referenceHashing

        TypeCycle ty2 ->
            typeReferences ty2

        TypeAbs _ ty2 ->
            typeReferences ty2

        TypeTm (TypeRef ref) ->
            HashSet.singleton referenceEquality referenceHashing ref

        TypeTm (TypeArrow ty1 ty2) ->
            HashSet.union (typeReferences ty1) (typeReferences ty2)

        TypeTm (TypeAnn ty2 _) ->
            typeReferences ty2

        TypeTm (TypeApp ty1 ty2) ->
            HashSet.union (typeReferences ty1) (typeReferences ty2)

        TypeTm (TypeEffect ty1 ty2) ->
            HashSet.union (typeReferences ty1) (typeReferences ty2)

        TypeTm (TypeEffects tys) ->
            HashSet.unions
                referenceEquality
                referenceHashing
                (List.map typeReferences tys)

        TypeTm (TypeForall ty) ->
            typeReferences ty

        TypeTm (TypeIntroOuter ty) ->
            typeReferences ty


{-| Haskell function: Unison.Type.flattenEffects
-}
typeFlattenEffects : Type var -> List (Type var)
typeFlattenEffects ty =
    case ty.out of
        TypeTm (TypeEffects tys) ->
            List.concatMap typeFlattenEffects tys

        _ ->
            List.singleton ty


{-| Haskell function: Unison.Type.unApps
-}
typeUnApps : Type var -> Maybe ( Type var, List (Type var) )
typeUnApps ty0 =
    let
        go : Type var -> List (Type var) -> List (Type var)
        go ty acc =
            case ty.out of
                TypeTm (TypeApp ty1 ty2) ->
                    go ty1 (ty2 :: acc)

                _ ->
                    ty :: acc
    in
    case go ty0 [] of
        x :: y :: ys ->
            Just ( x, y :: ys )

        _ ->
            Nothing


{-| Haskell function: Unison.Type.unEffectfulArrows
Difference: this function takes the rhs of the first arrow, not the whole thing
-}
typeUnEffectfulArrows : Type var -> List ( Maybe (List (Type var)), Type var )
typeUnEffectfulArrows ty =
    case ty.out of
        TypeTm (TypeEffect ty1 ty2) ->
            case ty1.out of
                TypeTm (TypeEffects es) ->
                    let
                        es2 : Maybe (List (Type var))
                        es2 =
                            Just (List.concatMap typeFlattenEffects es)
                    in
                    case ty2.out of
                        TypeTm (TypeArrow ty3 ty4) ->
                            ( es2, ty3 ) :: typeUnEffectfulArrows ty4

                        _ ->
                            [ ( es2, ty2 ) ]

                _ ->
                    [ ( Nothing, ty ) ]

        TypeTm (TypeArrow ty3 ty4) ->
            ( Nothing, ty3 ) :: typeUnEffectfulArrows ty4

        _ ->
            [ ( Nothing, ty ) ]


{-| Haskell type: Unison.Type.unForalls
-}
typeUnForalls : List var -> Type var -> ( List var, Type var )
typeUnForalls vars ty =
    case ty.out of
        TypeTm (TypeForall ty2) ->
            case ty2.out of
                TypeAbs var ty3 ->
                    typeUnForalls (var :: vars) ty3

                _ ->
                    impossible "unForalls: forall not followed by abs"

        _ ->
            ( List.reverse vars, ty )


typeUnTuple :
    Type var
    -> List (Type var)
typeUnTuple ty =
    case ty.out of
        TypeTm (TypeRef ref) ->
            if ref == unitReference then
                []

            else
                impossible "typeUnTuple: didn't end with ()?"

        _ ->
            case typeUnApps ty of
                Just ( f, [ ty1, ty2 ] ) ->
                    case f.out of
                        TypeTm (TypeRef ref) ->
                            if ref == pairReference then
                                ty1 :: typeUnTuple ty2

                            else
                                impossible "typeUnTuple: not pair?"

                        _ ->
                            impossible "typeUnTuple: not pair?"

                _ ->
                    impossible "typeUnTuple: wat?"


typeIsPairRef :
    Type var
    -> Bool
typeIsPairRef ty =
    ty.out == TypeTm (TypeRef pairReference)


typeIsSequenceRef :
    Type var
    -> Bool
typeIsSequenceRef ty =
    case ty.out of
        TypeTm (TypeRef (Builtin "Sequence")) ->
            True

        _ ->
            False
