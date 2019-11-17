module Ucb.Unison.Codebase.API.LocalServer exposing (makeLocalServerUnisonCodebaseAPI)

import Bytes exposing (Bytes)
import Json.Decode
import Task exposing (Task)
import Ucb.Unison.Codebase.API exposing (..)
import Ucb.Util.Http as Http
import Ucb.Util.Task as Task
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.Patch exposing (..)
import Unison.Codebase.Serialization.V1 as V1
import Unison.Declaration exposing (..)
import Unison.Reference exposing (..)
import Unison.Symbol exposing (Symbol)
import Unison.Term exposing (Term)
import Unison.Type exposing (..)
import Url exposing (..)
import Url.Builder as UrlBuilder exposing (..)


{-| Haskell server localhost base url
-}
devBase : Url
devBase =
    { protocol = Http
    , host = "localhost"
    , port_ = Just 8180
    , path = ""
    , query = Nothing
    , fragment = Nothing
    }



--  "http://localhost:8180/"


prefixIfDev : Bool -> String -> String
prefixIfDev isDev path =
    if isDev then
        Url.toString { devBase | path = "/" ++ path }

    else
        UrlBuilder.absolute [ path ] []


makeLocalServerUnisonCodebaseAPI : Bool -> UnisonCodebaseAPI
makeLocalServerUnisonCodebaseAPI isDev =
    { getHeadHash = getHeadHash isDev
    , getPatch = getPatch isDev
    , getRawCausal = getRawCausal isDev
    , getTerm = getTerm isDev
    , getTermType = getTermType isDev
    , getTypeDecl = getTypeDecl isDev
    }


getHeadHash : Bool -> Task (Http.Error String) (Http.Response BranchHash)
getHeadHash isDev =
    Http.getJson
        { decoder = Json.Decode.string
        , headers = []
        , timeout = Nothing
        , url = prefixIfDev isDev "api/head"
        }


getPatch :
    Bool
    -> PatchHash
    -> Task (Http.Error Bytes) ( PatchHash, Http.Response Patch )
getPatch isDev hash =
    Http.getBytes
        { decoder = V1.patchDecoder
        , headers = []
        , timeout = Nothing
        , url = prefixIfDev isDev ("api/patch/" ++ hash)
        }
        |> Task.map (\response -> ( hash, response ))


getRawCausal :
    Bool
    -> BranchHash
    -> Task (Http.Error Bytes) ( BranchHash, Http.Response (RawCausal RawBranch) )
getRawCausal isDev hash =
    Http.getBytes
        { decoder = V1.rawCausalDecoder
        , headers = []
        , timeout = Nothing
        , url = prefixIfDev isDev ("api/branch/" ++ hash)
        }
        |> Task.map (\response -> ( hash, response ))


getTerm :
    Bool
    -> Id
    -> Task (Http.Error Bytes) ( Id, Http.Response (Term Symbol) )
getTerm isDev id =
    Http.getBytes
        { decoder = V1.termDecoder
        , headers = []
        , timeout = Nothing
        , url = prefixIfDev isDev ("api/term/" ++ idToString id ++ "/term")
        }
        |> Task.map (\response -> ( id, response ))


getTermType :
    Bool
    -> Id
    -> Task (Http.Error Bytes) ( Id, Http.Response (Type Symbol) )
getTermType isDev id =
    Http.getBytes
        { decoder = V1.typeDecoder
        , headers = []
        , timeout = Nothing
        , url = prefixIfDev isDev ("api/term/" ++ idToString id ++ "/type")
        }
        |> Task.map (\response -> ( id, response ))


getTypeDecl :
    Bool
    -> Id
    -> Task (Http.Error Bytes) ( Id, Http.Response (Declaration Symbol) )
getTypeDecl isDev id =
    Http.getBytes
        { decoder = V1.declarationDecoder
        , headers = []
        , timeout = Nothing
        , url = prefixIfDev isDev ("api/declaration/" ++ idToString id)
        }
        |> Task.map (\response -> ( id, response ))
