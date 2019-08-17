module Unison.Codebase.TypeEdit exposing (..)

import Unison.Reference exposing (Reference)


{-| Haskell type: Unison.Codebase.TypeEdit.TypeEdit
-}
type TypeEdit
    = TypeEditReplace Reference
    | TypeEditDeprecate
