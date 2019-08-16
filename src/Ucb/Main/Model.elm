module Ucb.Main.Model exposing (..)

import Bytes exposing (Bytes)
import Ucb.Unison.Codebase.Path exposing (GetHeadPathError)


type alias Model =
    { result : Maybe (Result GetHeadPathError Bytes)
    }
