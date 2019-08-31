module Ucb.Unison.Codebase.API.GitHub exposing (makeGitHubUnisonCodebaseAPI)

import Array exposing (Array)
import Bytes exposing (Bytes)
import GitHub
import Misc exposing (impossible)
import Task exposing (Task)
import Ucb.Unison.Codebase.API exposing (..)
import Ucb.Util.Http as Http
import Ucb.Util.Task as Task
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.Serialization.V1 as V1
import Unison.Declaration exposing (..)
import Unison.Reference exposing (..)
import Unison.Symbol exposing (Symbol)


makeGitHubUnisonCodebaseAPI :
    String
    -> String
    -> UnisonCodebaseAPI
makeGitHubUnisonCodebaseAPI owner repo =
    { getHeadHash = getHeadHash owner repo
    , getRawCausal = getRawCausal owner repo
    , getPatch = Debug.todo "GitHub getPatch"
    , getTerm = Debug.todo "GitHub getTerm"
    , getTermType = Debug.todo "GitHub getTermType"
    , getTypeDecl = getTypeDecl owner repo
    }


getHeadHash :
    String
    -> String
    -> Task (Http.Error String) (Http.Response String)
getHeadHash owner repo =
    GitHub.getRepoContents owner repo ".unison/v1/paths/_head"
        |> Task.map
            (\response ->
                { url = response.url
                , statusCode = response.statusCode
                , statusText = response.statusText
                , headers = response.headers
                , body = parseHeadHash response.body
                }
            )


parseHeadHash :
    GitHub.RepoContents
    -> BranchHash
parseHeadHash contents =
    case contents of
        GitHub.FileContents _ ->
            impossible "_head was a file?"

        GitHub.DirectoryContents dirents ->
            parseHeadHash2 dirents


parseHeadHash2 :
    Array GitHub.Dirent
    -> String
parseHeadHash2 dirents =
    case ( Array.get 0 dirents, Array.length dirents > 1 ) of
        ( Just dirent, False ) ->
            dirent.name

        _ ->
            impossible "_head didn't contain one file?"


getRawCausal :
    String
    -> String
    -> BranchHash
    -> Task (Http.Error Bytes) ( BranchHash, Http.Response (RawCausal RawBranch) )
getRawCausal owner repo hash =
    GitHub.getFile
        { owner = owner
        , repo = repo
        , ref = "master" -- TODO probably want to get default branch somehow?
        , path = ".unison/v1/paths/" ++ hash ++ ".ub"
        , decoder = V1.rawCausalDecoder
        }
        |> Task.map (\response -> ( hash, response ))


getTypeDecl :
    String
    -> String
    -> Id
    -> Task (Http.Error Bytes) ( Id, Http.Response (Declaration Symbol) )
getTypeDecl owner repo id =
    GitHub.getFile
        { owner = owner
        , repo = repo
        , ref = "master" -- TODO probably want to get default branch somehow?
        , path = ".unison/v1/types/%23" ++ idToString id ++ "/compiled.ub"
        , decoder = V1.declarationDecoder
        }
        |> Task.map (\response -> ( id, response ))
