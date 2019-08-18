module Ucb.Main.Message exposing (..)

import Ucb.Unison.Codebase.Path exposing (..)
import Ucb.Util.Http as Http
import Unison.Codebase.Causal exposing (..)
import Unison.Hash exposing (..)


type Message
    = User_GetBranch { hash : Hash32, focus : Bool }
    | Http_GetHeadHash (Result GetHeadHashError (Http.Response Hash32))
    | Http_GetRawCausal (Result GetRawCausalError ( Hash32, Http.Response RawCausal ))
