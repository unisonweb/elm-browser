module Ucb.Main.Message exposing (..)

import Bytes exposing (Bytes)
import Ucb.Util.Http as Http
import Unison.Codebase.Causal exposing (..)
import Unison.Declaration exposing (..)
import Unison.Hash exposing (..)
import Unison.Reference exposing (..)
import Unison.Referent exposing (..)
import Unison.Symbol exposing (..)
import Unison.Term exposing (..)
import Unison.Type exposing (..)


type Message
    = User_GetBranch { hash : Hash32, focus : Bool }
    | User_GetTerm Referent
    | User_GetType Reference
    | Http_GetHeadHash (Result (Http.Error String) (Http.Response Hash32))
    | Http_GetRawCausal (Result (Http.Error Bytes) ( Hash32, Http.Response RawCausal ))
    | Http_GetTerm (Result (Http.Error Bytes) ( Id, Http.Response (Term Symbol) ))
    | Http_GetTermType (Result (Http.Error Bytes) ( Id, Http.Response (Type Symbol) ))
    | Http_GetType (Result (Http.Error Bytes) ( Id, Http.Response (Declaration Symbol) ))
