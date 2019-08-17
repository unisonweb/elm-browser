module Unison.Type exposing (..)

import HashingContainers.HashSet exposing (HashSet)
import Unison.Kind exposing (Kind)
import Unison.Parser exposing (Ann)
import Unison.Reference exposing (Reference)
import Unison.Symbol exposing (Symbol)


{-| Haskell type: Unison.Type.Type
-}
type alias Type =
    { freeVars : HashSet Symbol
    , annotation : Ann
    , out : () -- TODO whoops this seems hard, come back later...
    }
