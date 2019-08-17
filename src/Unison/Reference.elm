module Unison.Reference exposing
    ( Id
    , Reference(..)
    )

import Bytes.Encode
import HashingContainers exposing (Equality, Hashing)
import Unison.Hash exposing (Hash)


{-| Haskell type: Unison.Reference.Reference
-}
type Reference
    = Builtin String
    | Derived Id


{-| Just don't ask.
-}
type alias ReferenceOrdering =
    ( Int, String, Id )


{-| Haskell type: Unison.Reference.Id
-}
type alias Id =
    { hash : Hash
    , pos : Int
    , size : Int
    }


referenceEquality : HashingContainers.Equality Reference
referenceEquality =
    HashingContainers.eq
        (\rx ry ->
            case ( rx, ry ) of
                ( Builtin x, Builtin y ) ->
                    x == y

                ( Derived x, Derived y ) ->
                    -- hash implies size
                    x.hash == y.hash && x.pos == y.pos

                _ ->
                    False
        )


referenceHashing : HashingContainers.Hashing Reference
referenceHashing =
    Debug.todo ""
