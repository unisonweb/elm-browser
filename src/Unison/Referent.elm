module Unison.Referent exposing (Referent(..))

import Unison.ConstructorType exposing (..)
import Unison.Reference exposing (..)


{-| Haskell type: Unison.Referent.Referent
-}
type Referent
    = Ref Reference
    | Con Reference Int ConstructorType
