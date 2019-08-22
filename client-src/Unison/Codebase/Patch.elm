module Unison.Codebase.Patch exposing (..)

import Unison.Codebase.TermEdit exposing (TermEdit)
import Unison.Codebase.TypeEdit exposing (TypeEdit)
import Unison.Reference exposing (Reference)
import Unison.Util.Relation exposing (Relation)


{-| Haskell type: Unison.Codebase.Patch.Patch
-}
type alias Patch =
    { termEdits : Relation Reference TermEdit
    , typeEdits : Relation Reference TypeEdit
    }
