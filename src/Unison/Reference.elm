module Unison.Reference exposing
    ( Id
    , Reference(..)
    , ReferenceOrdering
    , bogusReferenceOrdering
    , referenceOrdering
    )

import Bytes.Encode
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


bogusId : Id
bogusId =
    { hash = Bytes.Encode.encode (Bytes.Encode.sequence [])
    , pos = 0
    , size = 0
    }


referenceOrdering : Reference -> ReferenceOrdering
referenceOrdering reference =
    let
        bogusString =
            ""
    in
    case reference of
        Builtin string ->
            ( 0, string, bogusId )

        Derived id ->
            ( 1, bogusString, id )


bogusReferenceOrdering : ReferenceOrdering
bogusReferenceOrdering =
    ( -1, "", bogusId )
