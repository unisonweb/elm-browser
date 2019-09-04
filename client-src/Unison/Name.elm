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

-}
type Name
    = Name (Array NameSegment)


unsafeMakeName : Array NameSegment -> Name
unsafeMakeName =
    Name


makeName : Array NameSegment -> Maybe Name
makeName nameSegmentArray =
    let
        length =
            Array.length nameSegmentArray
    in
    case length of
        0 ->
            Nothing

        _ ->
            Just (Name nameSegmentArray)


empty : Name
empty =
    Name Array.empty


nameToNameSegments : Name -> Array NameSegment
nameToNameSegments name =
    case name of
        Name array ->
            array


nameEquality : Equality Name
nameEquality =
    Equality.map nameToNameSegments (Equality.array nameSegmentEquality)


nameHashing : Hashing Name
nameHashing =
    Hashing.map nameToNameSegments (Hashing.array nameSegmentHashing 10)


{-| TODO make this more efficient
-}
nameCompare :
    Name
    -> Name
    -> Order
nameCompare n1 n2 =
    compare (nameToString n1) (nameToString n2)


nameToString : Name -> String
nameToString =
    nameToNameSegments >> Array.toList >> String.join "."


nameFromNameSegment : NameSegment -> Name
nameFromNameSegment =
    Array.singleton >> unsafeMakeName


cons : NameSegment -> Name -> Name
cons nameSegment name =
    Array.append (Array.singleton nameSegment) (nameToNameSegments name) |> unsafeMakeName


last : Name -> NameSegment
last name =
    let
        array =
            nameToNameSegments name

        index =
            Array.length array - 1

        maybeLast =
            Array.get index array
    in
    case maybeLast of
        Nothing ->
            impossible "Received empty array"

        Just nameSegment ->
            nameSegment


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
        array =
            nameToNameSegments name

        n : Int
        n =
            Array.length array

        loop : Int -> List Name -> List Name
        loop m acc =
            if m == 0 then
                unsafeMakeName array :: acc

            else
                loop (m - 1) (unsafeMakeName (Array.slice m n array) :: acc)
    in
    loop (n - 1) []
