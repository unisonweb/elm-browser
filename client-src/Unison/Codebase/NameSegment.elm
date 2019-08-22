module Unison.Codebase.NameSegment exposing
    ( NameSegment
    , nameSegmentEquality
    , nameSegmentHashing
    )

import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)


{-| Haskell type: Unison.Codebase.NameSegment.NameSegment
-}
type alias NameSegment =
    String


nameSegmentEquality : Equality NameSegment
nameSegmentEquality =
    Equality.string


nameSegmentHashing : Hashing NameSegment
nameSegmentHashing =
    -- Use the whole thing
    Hashing.string 100
