module Unison.Codebase.TermEdit exposing (..)

import Typeclasses.Classes.Equality exposing (Equality)
import Typeclasses.Classes.Hashing exposing (Hashing)
import Unison.Reference exposing (Reference)


{-| Haskell type: Unison.Codebase.TermEdit.TermEdit
-}
type TermEdit
    = TermEditReplace Reference Typing
    | TermEditDeprecate


termEditEquality : Equality TermEdit
termEditEquality =
    Debug.todo "termEditEquality"


termEditHashing : Hashing TermEdit
termEditHashing =
    Debug.todo "termEditHashing"


{-| Haskell type: Unison.Codebase.TermEdit.Typing
-}
type Typing
    = Same
    | Subtype
    | Different
