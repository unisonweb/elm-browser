module Unison.Blank exposing (..)

import Basics


{-| Haskell type: Unison.Blank.Blank
-}
type Blank a
    = Blank
    | Recorded (Recorded a)


{-| Haskell type: Unison.Blank.Recorded
-}
type Recorded a
    = Placeholder a String
    | Resolve a String
