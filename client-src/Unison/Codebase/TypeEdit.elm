module Unison.Codebase.TypeEdit exposing (..)

import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import Unison.Reference exposing (..)


{-| Haskell type: Unison.Codebase.TypeEdit.TypeEdit
-}
type TypeEdit
    = TypeEditReplace Reference
    | TypeEditDeprecate


typeEditEquality : Equality TypeEdit
typeEditEquality =
    Equality.eq (==)


typeEditHashing : Hashing TypeEdit
typeEditHashing =
    Hashing.hash
        (\edit ->
            case edit of
                TypeEditReplace reference ->
                    referenceHashing.hash reference

                TypeEditDeprecate ->
                    1
        )
