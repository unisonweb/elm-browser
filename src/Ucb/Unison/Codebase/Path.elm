-- Utilities that have to do with the .unison/v1/paths folder. Currently
-- hard-coded to fetch from GitHub.


module Ucb.Unison.Codebase.Path exposing
    ( GetHeadPathError(..)
    , httpGetHeadPath
    )

import Array exposing (Array)
import Bytes exposing (Bytes)
import Http
import Task exposing (Task)
import Ucb.GitHub
import Ucb.Util.Task as Task


{-| Something went wrong when getting the head path.
TODO(mitchell) change name to GetPathError
-}
type GetHeadPathError
    = GetHeadPathError_Http Http.Error
    | GetHeadPathError_Other String


{-| Given an owner (like "unisonweb") and a repo (like "unisonbase", return the
head namespace hash, that is, the name of the file located at
.unison/v1/paths/\_head/<namespace-hash>

According to Arya in Slack, this should normally contain exactly one file. So we
assert that here, and fail if it isn't the case, but perhaps there is something
smarter to do instead.

-}
httpGetHeadPath :
    String
    -> String
    -> Task GetHeadPathError Bytes
httpGetHeadPath owner repo =
    Ucb.GitHub.getRepoContents owner repo ".unison/v1/paths/_head"
        |> Task.mapError GetHeadPathError_Http
        |> Task.andThen (parseHeadPath >> Task.fromResult)
        |> Task.andThen
            (\path ->
                httpGetPath owner repo (".unison/v1/paths/" ++ path ++ ".ub")
            )


parseHeadPath :
    Ucb.GitHub.RepoContents
    -> Result GetHeadPathError String
parseHeadPath contents =
    case contents of
        Ucb.GitHub.FileContents _ ->
            Err (GetHeadPathError_Other "expected dir, found file")

        Ucb.GitHub.DirectoryContents dirents ->
            parseHeadPath2 dirents


parseHeadPath2 : Array Ucb.GitHub.Dirent -> Result GetHeadPathError String
parseHeadPath2 dirents =
    case ( Array.get 0 dirents, Array.length dirents > 1 ) of
        ( Just dirent, False ) ->
            Ok dirent.name

        _ ->
            Err (GetHeadPathError_Other "expected only one path")


httpGetPath :
    String
    -> String
    -> String
    -> Task GetHeadPathError Bytes
httpGetPath owner repo path =
    Ucb.GitHub.httpGetRawFile owner repo path
        |> Task.mapError GetHeadPathError_Http
