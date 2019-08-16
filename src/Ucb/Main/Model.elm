module Ucb.Main.Model exposing (..)

import Ucb.Unison.Codebase.Path exposing (GetHeadPathError)


type alias Model =
    { result : Maybe (Result GetHeadPathError String)
    }
