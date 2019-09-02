module Unison.Name exposing (..)

import Array exposing (Array)
import Misc exposing (..)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import Ucb.Util.Array as Array
import Unison.Codebase.NameSegment exposing (..)


{-| We deviate from the current Unison codebase here... this may be a better
"Name" type for our purposes.

Invariant: non-empty

TODO(elliott) make this an opaque type, don't export its constructor

-}
type alias Name =
    Array NameSegment


nameEquality : Equality Name
nameEquality =
    Equality.array nameSegmentEquality


nameHashing : Hashing Name
nameHashing =
    Hashing.array nameSegmentHashing 10


nameToString : Name -> String
nameToString =
    Array.toList >> String.join "."


nameFromNameSegment : NameSegment -> Name
nameFromNameSegment =
    Array.singleton


{-|

> nameTails ["foo", "bar", "baz"]

    [ [ "foo", "bar", "baz" ]
    , [ "bar", "baz" ]
    , [ "baz" ]
    ]

-}
nameTails : Name -> List Name
nameTails name =
    let
        n : Int
        n =
            Array.length name

        loop : Int -> List Name -> List Name
        loop m acc =
            if m == 0 then
                name :: acc

            else
                loop (m - 1) (Array.slice m n name :: acc)
    in
    loop (n - 1) []
