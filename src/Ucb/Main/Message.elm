module Ucb.Main.Message exposing (..)

import Ucb.Unison.Codebase.Path exposing (GetHeadPathError)


type Message
    = DownloadedHeadPath (Result GetHeadPathError String)
