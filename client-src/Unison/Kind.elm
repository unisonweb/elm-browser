module Unison.Kind exposing (..)

{-| Haskell type: Unison.Kind.Kind
-}


type Kind
    = Star
    | Arrow Kind Kind
