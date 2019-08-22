module Ucb.Unison.Codebase.API.LocalServer exposing (makeLocalServerUnisonCodebaseAPI)

import Array exposing (Array)
import Bytes exposing (Bytes)
import Json.Decode
import Task exposing (Task)
import Ucb.Unison.Codebase.API exposing (..)
import Ucb.Util.Http as Http
import Ucb.Util.Task as Task
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.Serialization.V1 as V1
import Unison.Declaration exposing (..)
import Unison.Hash exposing (Hash32)
import Unison.Reference exposing (..)
import Unison.Symbol exposing (Symbol)


makeLocalServerUnisonCodebaseAPI : UnisonCodebaseAPI
makeLocalServerUnisonCodebaseAPI =
    { getHeadHash = getHeadHash
    , getRawCausal = getRawCausal
    , getType = getType
    }


getHeadHash : Task GetHeadHashError (Http.Response Hash32)
getHeadHash =
    Http.getJson
        { decoder = Json.Decode.string
        , headers = []
        , timeout = Nothing
        , url = "head"
        }
        |> Task.mapError GetHeadHashError_Http


getRawCausal :
    Hash32
    -> Task GetRawCausalError ( Hash32, Http.Response RawCausal )
getRawCausal hash =
    Http.getBytes
        { decoder = V1.rawCausalDecoder
        , headers = []
        , timeout = Nothing
        , url = "branch/" ++ hash
        }
        |> Task.mapError GetRawCausalError_Http
        |> Task.map (\response -> ( hash, response ))


getType :
    Id
    -> Task GetTypeError ( Id, Http.Response (Declaration Symbol) )
getType id =
    Http.getBytes
        { decoder = V1.declarationDecoder
        , headers = []
        , timeout = Nothing
        , url = "declaration/" ++ idToString id
        }
        |> Task.mapError GetTypeError_Http
        |> Task.map (\response -> ( id, response ))
