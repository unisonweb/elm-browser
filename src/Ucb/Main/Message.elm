module Ucb.Main.Message exposing (..)

import Ucb.Unison.Codebase exposing (Codebase, GetCodebaseError)


type Message
    = DownloadedCodebase (Result GetCodebaseError Codebase)
