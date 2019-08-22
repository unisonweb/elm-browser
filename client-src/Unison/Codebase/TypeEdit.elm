module Unison.Codebase.TypeEdit exposing (..)

import Typeclasses.Classes.Equality exposing (Equality)
import Typeclasses.Classes.Hashing exposing (Hashing)
import Unison.Reference exposing (Reference)


{-| Haskell type: Unison.Codebase.TypeEdit.TypeEdit
-}
type TypeEdit
    = TypeEditReplace Reference
    | TypeEditDeprecate


typeEditEquality : Equality TypeEdit
typeEditEquality =
    Debug.todo "typeEditEquality"


typeEditHashing : Hashing TypeEdit
typeEditHashing =
    Debug.todo "typeEditHashing"
