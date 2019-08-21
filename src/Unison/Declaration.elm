module Unison.Declaration exposing (..)

import Unison.Type exposing (Type)


{-| Haskell type: Unison.DataDeclaration.Decl
-}
type Declaration var
    = DataDecl (DataDeclaration var)
    | EffectDecl (DataDeclaration var)


type alias DataDeclaration var =
    { modifier : Modifier
    , bound : List var
    , constructors : List ( var, Type var )
    }


{-| Haskell type: Unison.DataDeclaration.Modifier
-}
type Modifier
    = Structural
    | Unique String
