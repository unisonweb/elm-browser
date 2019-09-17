module Unison.Blank exposing (..)

{-| Haskell type: Unison.Blank.Blank
-}


type Blank
    = Blank
    | Recorded Recorded


{-| Haskell type: Unison.Blank.Recorded
-}
type Recorded
    = Placeholder String
    | Resolve String
