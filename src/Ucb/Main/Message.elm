module Ucb.Main.Message exposing (..)

import Bytes exposing (Bytes)
import Ucb.Unison.Codebase.Path exposing (GetHeadPathError)


type Message
    = DownloadedHeadPath (Result GetHeadPathError Bytes)
