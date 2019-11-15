module Unison.Reference exposing
    ( Id
    , Reference(..)
    , idEquality
    , idFromString
    , idHashing
    , idToString
    , referenceEquality
    , referenceHashing
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


idFromString : String -> Maybe Id
idFromString string =
    let
        parsed =
            String.split "." string
    in
    case parsed of
        [] ->
            Nothing

        "" :: xs ->
            Nothing

        [ hash ] ->
            Just { hash = hash, pos = 0, size = 1 }

        hash :: xs ->
            let
                posSizeList =
                    List.concatMap (String.split "c") xs
            in
            case posSizeList of
                [] ->
                    Just { hash = hash, pos = 0, size = 1 }

                maybePos :: sizeSingleton ->
                    let
                        pos =
                            Maybe.withDefault 0 (String.toInt maybePos)

                        size =
                            Maybe.withDefault 1 (String.toInt (Maybe.withDefault "1" (List.head sizeSingleton)))
                    in
                    Just { hash = hash, pos = pos, size = size }
