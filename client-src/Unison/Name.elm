module Unison.Name exposing (..)

import Misc exposing (..)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import Unison.Codebase.NameSegment exposing (..)


{-| We deviate from the current Unison codebase here... this may be a better
"Name" type for our purposes.

Invariant: non-empty

-}
type alias Name =
    List NameSegment


nameEquality : Equality Name
nameEquality =
    Equality.list nameSegmentEquality


nameHashing : Hashing Name
nameHashing =
    Hashing.list nameSegmentHashing


{-|

> nameTails ["foo", "bar", "baz"]

    [ [ "foo", "bar", "baz" ]
    , [ "bar", "baz" ]
    , [ "baz" ]
    ]

-}
nameTails : Name -> List Name
nameTails name =
    case name of
        [] ->
            impossible "nameTails: empty name"

        [ x ] ->
            [ [ x ] ]

        x :: xs ->
            (x :: xs) :: nameTails xs
