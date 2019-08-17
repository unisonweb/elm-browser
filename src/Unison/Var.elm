module Unison.Var exposing (..)

import Unison.Reference exposing (Reference)


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
