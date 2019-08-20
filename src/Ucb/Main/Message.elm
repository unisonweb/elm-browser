module Ucb.Main.Message exposing (..)

import Ucb.Unison.Codebase.Path exposing (..)
import Ucb.Unison.Codebase.Type exposing (..)
import Ucb.Util.Http as Http
import Unison.Codebase.Causal exposing (..)
import Unison.Hash exposing (..)
import Unison.Reference exposing (..)
import Unison.Symbol exposing (..)
import Unison.Type exposing (..)


type Message
    = User_GetBranch { hash : Hash32, focus : Bool }
    | User_GetType Reference
    | Http_GetHeadHash (Result GetHeadHashError (Http.Response Hash32))
    | Http_GetRawCausal (Result GetRawCausalError ( Hash32, Http.Response RawCausal ))
    | Http_GetType (Result GetTypeError ( Id, Http.Response (Type Symbol) ))
