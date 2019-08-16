module Unison.Referent exposing
    ( Referent(..)
    , ReferentOrdering
    , referentOrdering
    )

import Unison.ConstructorType exposing (..)
import Unison.Reference exposing (..)


{-| Haskell type: Unison.Referent.Referent
-}
type Referent
    = Ref Reference
    | Con Reference Int ConstructorType


type alias ReferentOrdering =
    ( Int, ReferenceOrdering, ( Int, ConstructorTypeOrdering ) )


referentOrdering : Referent -> ReferentOrdering
referentOrdering referent =
    let
        bogusConstructorId =
            -1
    in
    case referent of
        Ref reference ->
            ( 0
            , referenceOrdering reference
            , ( bogusConstructorId
              , bogusConstructorTypeOrdering
              )
            )

        Con reference id constructorType ->
            ( 1
            , bogusReferenceOrdering
            , ( id
              , constructorTypeOrdering constructorType
              )
            )
