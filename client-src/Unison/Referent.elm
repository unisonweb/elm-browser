module Unison.Referent exposing
    ( Referent(..)
    , referentEquality
    , referentHashing
    )

import Misc exposing (tumble)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import Unison.ConstructorType exposing (..)
import Unison.Reference exposing (..)


{-| Haskell type: Unison.Referent.Referent
-}
type Referent
    = Ref Reference
    | Con Reference Int ConstructorType


referentEquality : Equality Referent
referentEquality =
    Equality.eq
        (\rx ry ->
            case ( rx, ry ) of
                ( Ref x, Ref y ) ->
                    referenceEquality.eq x y

                ( Con refX idX _, Con refY idY _ ) ->
                    -- reference implies constructor type
                    referenceEquality.eq refX refY && idX == idY

                _ ->
                    False
        )


referentHashing : Hashing Referent
referentHashing =
    Hashing.hash
        (\referent ->
            case referent of
                Ref ref ->
                    referenceHashing.hash ref

                Con ref id _ ->
                    1
                        |> tumble (referenceHashing.hash ref)
                        |> tumble id
        )
