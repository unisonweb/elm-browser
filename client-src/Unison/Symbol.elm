module Unison.Symbol exposing (..)

import Misc exposing (tumble)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import Unison.Var exposing (..)
import Word64 exposing (..)


{-| Unison type: Unison.Symbol.Symbol
-}
type Symbol
    = Symbol Word64 VarType


symbolEquality : Equality Symbol
symbolEquality =
    Equality.eq
        (\(Symbol wa va) (Symbol wb vb) ->
            wa == wb && va == vb
        )


symbolHashing : Hashing Symbol
symbolHashing =
    Hashing.hash
        (\(Symbol w v) ->
            tumble
                (word64Hashing.hash w)
                (varTypeHashing.hash v)
        )


symbolToString :
    Symbol
    -> String
symbolToString symbol =
    case symbol of
        Symbol n var ->
            case var of
                User name ->
                    let
                        m : Int
                        m =
                            unsafeWord64ToWord53 n
                    in
                    if m == 0 then
                        name

                    else
                        name ++ String.fromInt m

                _ ->
                    "???"
