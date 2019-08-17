module Unison.Codebase.TermEdit exposing (..)

import Unison.Reference exposing (Reference)


{-| Haskell type: Unison.Codebase.TermEdit.TermEdit
-}
type TermEdit
    = TermEditReplace Reference Typing
    | TermEditDeprecate


{-| Haskell type: Unison.Codebase.TermEdit.Typing
-}
type Typing
    = Same
    | Subtype
    | Different
