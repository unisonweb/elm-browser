module Unison.Reference exposing
    ( Id
    , Reference(..)
    , idEquality
    , idHashing
    , idToString
    , pairReference
    , referenceEquality
    , referenceHashing
    , unitReference
    )

import Misc exposing (tumble)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import Unison.Hash exposing (..)


{-| Haskell type: Unison.Reference.Reference
-}
type Reference
    = Builtin String
    | Derived Id


referenceEquality : Equality Reference
referenceEquality =
    Equality.eq
        (\rx ry ->
            case ( rx, ry ) of
                ( Builtin x, Builtin y ) ->
                    x == y

                ( Derived x, Derived y ) ->
                    idEquality.eq x y

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


{-| Haskell type: Unison.Reference.Id
-}
type alias Id =
    { hash : Hash32
    , pos : Int
    , size : Int
    }


idEquality : Equality Id
idEquality =
    Equality.eq
        (\id1 id2 ->
            hash32Equality.eq id1.hash id2.hash
                && (id1.pos == id2.pos)
        )


idHashing : Hashing Id
idHashing =
    Hashing.hash
        (\{ hash, pos } ->
            hash32Hashing.hashWithSalt pos hash
        )


idToString :
    Id
    -> String
idToString { hash, pos, size } =
    if size == 1 then
        hash

    else
        hash
            ++ String.fromChar '.'
            ++ String.fromInt pos
            ++ String.fromChar 'c'
            ++ String.fromInt size


pairReference : Reference
pairReference =
    Derived
        { hash = "onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0"
        , pos = 0
        , size = 1
        }


unitReference : Reference
unitReference =
    Derived
        { hash = "568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8"
        , pos = 0
        , size = 1
        }
