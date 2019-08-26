module Unison.Var exposing (..)

import Misc exposing (tumble)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import Unison.Reference exposing (..)


{-| Haskell type: Unison.Var.Type
-}
type VarType
    = User String
    | Inference InferenceType
    | RefNamed Reference
    | MissingResult
    | AskInfo
    | Blank
    | UnnamedWatch String String


varTypeHashing : Hashing VarType
varTypeHashing =
    Hashing.hash
        (\v ->
            case v of
                User string ->
                    0
                        -- |> tumble ((Hashing.string 4).hash string)
                        |> tumble ((Hashing.string 100).hash string)

                Inference inferenceType ->
                    1
                        |> tumble (inferenceTypeHashing.hash inferenceType)

                RefNamed reference ->
                    2
                        |> tumble (referenceHashing.hash reference)

                MissingResult ->
                    3

                AskInfo ->
                    4

                Blank ->
                    5

                UnnamedWatch s t ->
                    6
                        |> tumble ((Hashing.string 100).hash s)
                        -- |> tumble ((Hashing.string 4).hash s)
                        |> tumble ((Hashing.string 100).hash t)
         -- |> tumble ((Hashing.string 4).hash t)
        )


{-| Haskell type: Unison.Var.InferenceType
-}
type InferenceType
    = Ability
    | Input
    | Output
    | PatternPureE
    | PatternPureV
    | PatternBindE
    | PatternBindV
    | TypeConstructor
    | TypeConstructorArg
    | Other


inferenceTypeHashing : Hashing InferenceType
inferenceTypeHashing =
    Hashing.hash
        (\inferenceType ->
            case inferenceType of
                Ability ->
                    0

                Input ->
                    1

                Output ->
                    2

                PatternPureE ->
                    3

                PatternPureV ->
                    4

                PatternBindE ->
                    5

                PatternBindV ->
                    6

                TypeConstructor ->
                    7

                TypeConstructorArg ->
                    8

                Other ->
                    9
        )
