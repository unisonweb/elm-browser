module Ucb.Unison.VType exposing
    ( VType(..)
    , VTypePath(..)
    , makeVType
    )

import Misc exposing (impossible, maybe)
import Unison.Reference exposing (..)
import Unison.Symbol exposing (..)
import Unison.Type exposing (..)


{-| A "view type". The purpose of this type is to simplify a Unison type down to
the bits that we actually care about for rendering.
-}
type VType
    = VTypeApp (List VType) -- Invariant: 2+ elements
    | VTypeArrows VType (List ( List VType, VType )) -- Invariant: 1+ elements
    | VTypeEffects (List VType) VType -- Invariant: 1+ elements
      -- TODO probably want 'List String' here?
    | VTypeForall (List Symbol) VType
    | VTypeRef Reference
    | VTypeSequence VType
    | VTypeVar String
      -- WHOT, shouldn't hit this ???,
      -- If you see this, we missed a case below
    | VType___UNKNOWN (Type Symbol)


type VTypePath
    = VTypePathHere
    | VTypePathIndex Int VTypePath
    | VTypePathLeft VTypePath
    | VTypePathRight VTypePath


makeVType :
    Type Symbol
    -> VType
makeVType ty0 =
    case ty0.out of
        TypeCycle ty ->
            makeVType ty

        TypeVar var ->
            VTypeVar (symbolToString var)

        TypeTm (TypeApp ty1 ty2) ->
            case ty1.out of
                TypeTm (TypeRef (Builtin "Sequence")) ->
                    VTypeSequence (makeVType ty2)

                _ ->
                    case typeUnApps ty0 of
                        Nothing ->
                            impossible "makeVType: typeUnApps returned Nothing"

                        Just ( f, xs ) ->
                            VTypeApp (List.map makeVType (f :: xs))

        TypeTm (TypeArrow ty1 ty2) ->
            VTypeArrows
                (makeVType ty1)
                (List.map
                    (\( effects, ty ) ->
                        ( maybe [] (List.map makeVType) effects
                        , makeVType ty
                        )
                    )
                    (typeUnEffectfulArrows ty2)
                )

        TypeTm (TypeEffect ty1 ty2) ->
            case ty1.out of
                TypeTm (TypeEffects effects) ->
                    VTypeEffects
                        (List.map makeVType effects)
                        (makeVType ty2)

                _ ->
                    VType___UNKNOWN ty0

        TypeTm (TypeForall _) ->
            let
                ( tyvars, ty ) =
                    typeUnForalls [] ty0
            in
            VTypeForall
                tyvars
                (makeVType ty)

        TypeTm (TypeIntroOuter ty) ->
            makeVType ty

        TypeTm (TypeRef reference) ->
            VTypeRef reference

        _ ->
            VType___UNKNOWN ty0
