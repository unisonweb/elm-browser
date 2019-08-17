module Unison.Reference exposing
    ( Id
    , Reference(..)
    , referenceEquality
    , referenceHashing
    )

import Bytes.Encode
import Misc exposing (tumble)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import Unison.Hash exposing (..)


{-| Haskell type: Unison.Reference.Reference
-}
type Reference
    = Builtin String
    | Derived Id


{-| Haskell type: Unison.Reference.Id
-}
type alias Id =
    { hash : Hash32
    , pos : Int
    , size : Int
    }


referenceEquality : Equality Reference
referenceEquality =
    Equality.eq
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


referenceHashing : Hashing Reference
referenceHashing =
    Hashing.hash
        (\reference ->
            case reference of
                Builtin name ->
                    -- 100 means "use whole str"
                    (Hashing.string 100).hash name

                Derived id ->
                    1
                        |> tumble (hashHash32 id.hash)
                        |> tumble id.pos
        )
