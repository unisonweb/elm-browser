-- Utilities that have to do with the .unison/v1/paths folder. Currently
-- hard-coded to fetch from GitHub.


module Ucb.Unison.Codebase.Path exposing
    ( GetHeadHashError(..)
    , GetRawCausalError(..)
    , httpGetHeadHash
    , httpGetRawCausal
    )

import Array exposing (Array)
import Bytes exposing (Bytes)
import Task exposing (Task)
import Ucb.GitHub
import Ucb.Util.Http as Http
import Ucb.Util.Task as Task
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.Serialization.V1 as V1
import Unison.Hash exposing (Hash32)


{-| Something went wrong when getting the head hash.
-}
type GetHeadHashError
    = GetHeadHashError_Http (Http.Error String)
    | GetHeadHashError_Other String


{-| Given an owner (like "unisonweb") and a repo (like "unisonbase", return the
head namespace hash, that is, the name of the file located at
.unison/v1/paths/\_head/<namespace-hash>

According to Arya in Slack, this should normally contain exactly one file. So we
assert that here, and fail if it isn't the case, but perhaps there is something
smarter to do instead.

-}
httpGetHeadHash :
    String
    -> String
    -> Task GetHeadHashError (Http.Response String)
httpGetHeadHash owner repo =
    Ucb.GitHub.httpGetRepoContents owner repo ".unison/v1/paths/_head"
        |> Task.mapError GetHeadHashError_Http
        |> Task.andThen httpGetHeadHash2


httpGetHeadHash2 :
    Http.Response Ucb.GitHub.RepoContents
    -> Task GetHeadHashError (Http.Response Hash32)
httpGetHeadHash2 response =
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
    Ucb.GitHub.RepoContents
    -> Result GetHeadHashError Hash32
parseHeadHash contents =
    case contents of
        Ucb.GitHub.FileContents _ ->
            Err (GetHeadHashError_Other "expected dir, found file")

        Ucb.GitHub.DirectoryContents dirents ->
            parseHeadHash2 dirents


parseHeadHash2 : Array Ucb.GitHub.Dirent -> Result GetHeadHashError String
parseHeadHash2 dirents =
    case ( Array.get 0 dirents, Array.length dirents > 1 ) of
        ( Just dirent, False ) ->
            Ok dirent.name

        _ ->
            Err (GetHeadHashError_Other "expected only one path")


{-| Something went wrong when getting a raw causal.
-}
type GetRawCausalError
    = GetRawCausalError_Http (Http.Error Bytes)
    | GetRawCausalError_Parse


httpGetRawCausal :
    String
    -> String
    -> Hash32
    -> Task GetRawCausalError ( Hash32, Http.Response RawCausal )
httpGetRawCausal owner repo hash =
    Ucb.GitHub.httpGetFile
        owner
        repo
        -- TODO probably want to get default branch somehow?
        "master"
        (".unison/v1/paths/" ++ hash ++ ".ub")
        V1.rawCausalDecoder
        |> Task.mapError GetRawCausalError_Http
        |> Task.map (\response -> ( hash, response ))
