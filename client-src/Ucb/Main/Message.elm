module Ucb.Main.Message exposing (..)

import Ucb.Unison.Codebase.API exposing (..)
import Ucb.Util.Http as Http
import Unison.Codebase.Causal exposing (..)
import Unison.Declaration exposing (..)
import Unison.Hash exposing (..)
import Unison.Reference exposing (..)
import Unison.Referent exposing (..)
import Unison.Symbol exposing (..)
import Unison.Term exposing (..)


type Message
    = User_GetBranch { hash : Hash32, focus : Bool }
    | User_GetTerm Referent
    | User_GetType Reference
    | Http_GetHeadHash (Result GetHeadHashError (Http.Response Hash32))
    | Http_GetRawCausal (Result GetRawCausalError ( Hash32, Http.Response RawCausal ))
    | Http_GetTerm (Result GetTermError ( Id, Http.Response (Term Symbol) ))
    | Http_GetType (Result GetTypeError ( Id, Http.Response (Declaration Symbol) ))
