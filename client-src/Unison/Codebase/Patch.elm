module Unison.Codebase.Patch exposing
    ( Patch
    , PatchHash
    )

import Unison.Codebase.TermEdit exposing (TermEdit)
import Unison.Codebase.TypeEdit exposing (TypeEdit)
import Unison.Hash exposing (Hash32)
import Unison.Reference exposing (Reference)
import Unison.Util.Relation exposing (Relation)


{-| Haskell type: Unison.Codebase.Patch.Patch
-}
type alias Patch =
    { termEdits : Relation Reference TermEdit
    , typeEdits : Relation Reference TypeEdit
    }


type alias PatchHash =
    Hash32
