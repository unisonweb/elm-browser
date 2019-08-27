module Unison.Symbol exposing (..)

import Misc exposing (..)
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


{-| Does this symbol begin with a lowercase letter?
-}
symbolIsLowercase :
    Symbol
    -> Bool
symbolIsLowercase (Symbol _ var) =
    case var of
        User s ->
            case String.uncons s of
                Nothing ->
                    impossible "symbolIsLowercase: empty symbol"

                -- FIXME this is inaccurate, but may be the best we can do in
                -- Elm... returns the wrong answer for uppercase letters that
                -- have no lowercase variant
                Just ( c, _ ) ->
                    Char.toLower c == c

        _ ->
            Debug.todo "symbolIsLowercase: non-User symbol"


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
                            unsafeWord64ToInt53 n
                    in
                    if m == 0 then
                        name

                    else
                        name ++ String.fromInt m

                _ ->
                    "???"
