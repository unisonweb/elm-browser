module Ucb.Main.Model exposing (..)

import Ucb.Unison.Codebase exposing (Codebase, GetCodebaseError)


type alias Model =
    { result : Maybe (Result GetCodebaseError Codebase)
    }
