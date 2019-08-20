module Ucb.Unison.Codebase.API.GitHub exposing (makeGitHubUnisonCodebaseAPI)

import Array exposing (Array)
import Bytes exposing (Bytes)
import GitHub
import Task exposing (Task)
import Ucb.Unison.Codebase.API exposing (..)
import Ucb.Util.Http as Http
import Ucb.Util.Task as Task
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.Serialization.V1 as V1
import Unison.Hash exposing (Hash32)
import Unison.Reference exposing (Id)
import Unison.Symbol exposing (Symbol)
import Unison.Type exposing (Type)


makeGitHubUnisonCodebaseAPI :
    String
    -> String
    -> UnisonCodebaseAPI
makeGitHubUnisonCodebaseAPI owner repo =
    { getHeadHash = getHeadHash owner repo
    , getRawCausal = getRawCausal owner repo
    , getType = getType owner repo
    }


getHeadHash :
    String
    -> String
    -> Task GetHeadHashError (Http.Response String)
getHeadHash owner repo =
    GitHub.getRepoContents owner repo ".unison/v1/paths/_head"
        |> Task.mapError GetHeadHashError_Http
        |> Task.andThen getHeadHash2


getHeadHash2 :
    Http.Response GitHub.RepoContents
    -> Task GetHeadHashError (Http.Response Hash32)
getHeadHash2 response =
    case parseHeadHash response.body of
        Err err ->
            Task.fail err

        Ok hash ->
            Task.succeed
                { url = response.url
                , statusCode = response.statusCode
                , statusText = response.statusText
                , headers = response.headers
                , body = hash
                }


parseHeadHash :
    GitHub.RepoContents
    -> Result GetHeadHashError Hash32
parseHeadHash contents =
    case contents of
        GitHub.FileContents _ ->
            Debug.todo ""

        GitHub.DirectoryContents dirents ->
            parseHeadHash2 dirents


parseHeadHash2 :
    Array GitHub.Dirent
    -> Result GetHeadHashError String
parseHeadHash2 dirents =
    case ( Array.get 0 dirents, Array.length dirents > 1 ) of
        ( Just dirent, False ) ->
            Ok dirent.name

        _ ->
            Err (GetHeadHashError_Other "expected only one path")


getRawCausal :
    String
    -> String
    -> Hash32
    -> Task GetRawCausalError ( Hash32, Http.Response RawCausal )
getRawCausal owner repo hash =
    GitHub.getFile
        { owner = owner
        , repo = repo
        , ref = "master" -- TODO probably want to get default branch somehow?
        , path = ".unison/v1/paths/" ++ hash ++ ".ub"
        , decoder = V1.rawCausalDecoder
        }
        |> Task.mapError GetRawCausalError_Http
        |> Task.map (\response -> ( hash, response ))


getType :
    String
    -> String
    -> Id
    -> Task GetTypeError ( Id, Http.Response (Type Symbol) )
getType owner repo id =
    GitHub.getFile
        { owner = owner
        , repo = repo
        , ref = "master" -- TODO probably want to get default branch somehow?
        , path = ".unison/v1/types/" ++ idToPath id
        , decoder = V1.typeDecoder
        }
        |> Task.mapError GetTypeError_Http
        |> Task.map (\response -> ( id, response ))


{-| Get the path in the types/ folder where a type is stored.
-}
idToPath :
    Id
    -> String
idToPath { hash, pos, size } =
    -- octothorp
    "%23"
        ++ (if size == 1 then
                hash

            else
                hash
                    ++ String.fromChar '.'
                    ++ String.fromInt (pos + 1)
                    ++ String.fromChar 'c'
                    ++ String.fromInt size
           )
        ++ "/compiled.ub"