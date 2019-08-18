module Ucb.Main.Message exposing (..)

import Ucb.Unison.Codebase.Path exposing (..)
import Ucb.Util.Http as Http
import Unison.Codebase.Causal exposing (..)
import Unison.Hash exposing (..)


type Message
    = ClickBranchHash Hash32
    | GetHeadHash (Result GetHeadHashError (Http.Response Hash32))
    | GetRawCausal (Result GetRawCausalError ( Hash32, Http.Response RawCausal ))
