module Unison.ConstructorType exposing
    ( ConstructorType(..)
    , ConstructorTypeOrdering
    , bogusConstructorTypeOrdering
    , constructorTypeOrdering
    )

{-| Haskell type: Unison.ConstructorType.ConstructorType
-}


type ConstructorType
    = Data
    | Effect


type alias ConstructorTypeOrdering =
    Int


constructorTypeOrdering : ConstructorType -> ConstructorTypeOrdering
constructorTypeOrdering constructorType =
    case constructorType of
        Data ->
            0

        Effect ->
            1


bogusConstructorTypeOrdering : ConstructorTypeOrdering
bogusConstructorTypeOrdering =
    -1
