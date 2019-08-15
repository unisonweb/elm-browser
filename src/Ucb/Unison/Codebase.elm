module Ucb.Unison.Codebase exposing
    ( Codebase
    , GetCodebaseError(..)
    , getCodebase
    )

import Array exposing (Array)
import Http
import Ucb.GitHub
import Ucb.Util.ArrayParser as ArrayParser exposing (ArrayParser)


type alias Codebase =
    { dependents : Ucb.GitHub.Dirent
    , patches : Ucb.GitHub.Dirent
    , paths : Ucb.GitHub.Dirent
    , terms : Ucb.GitHub.Dirent
    , type_index : Ucb.GitHub.Dirent
    , type_mentions_index : Ucb.GitHub.Dirent
    , types : Ucb.GitHub.Dirent
    }


type GetCodebaseError
    = GetCodebaseError_Http Http.Error
    | GetCodebaseError_Parse -- Could not parse the codebase dir. Informative!


{-| Download a v1 Unison codebase from GitHub
-}
getCodebase : String -> String -> Cmd (Result GetCodebaseError Codebase)
getCodebase owner repo =
    Cmd.map
        parseCodebase
        (Ucb.GitHub.getRepoContents owner repo ".unison/v1")


{-| Parse a v1 Unison codebase from the result of 'getRepoContents'
-}
parseCodebase :
    Result Http.Error Ucb.GitHub.RepoContents
    -> Result GetCodebaseError Codebase
parseCodebase result =
    case result of
        Err err ->
            Err (GetCodebaseError_Http err)

        Ok contents ->
            parseCodebase2 contents


parseCodebase2 : Ucb.GitHub.RepoContents -> Result GetCodebaseError Codebase
parseCodebase2 contents =
    case contents of
        Ucb.GitHub.FileContents _ ->
            Err GetCodebaseError_Parse

        Ucb.GitHub.DirectoryContents contents2 ->
            parseCodebase3 contents2


parseCodebase3 : Array Ucb.GitHub.Dirent -> Result GetCodebaseError Codebase
parseCodebase3 dirents =
    case ArrayParser.run codebaseParser dirents of
        Nothing ->
            Err GetCodebaseError_Parse

        Just codebase ->
            Ok codebase


codebaseParser : ArrayParser Ucb.GitHub.Dirent Codebase
codebaseParser =
    let
        get : String -> Ucb.GitHub.Dirent -> Maybe Ucb.GitHub.Dirent
        get name dirent =
            if dirent.name == name then
                Just dirent

            else
                Nothing
    in
    ArrayParser.pure Codebase
        |> ArrayParser.parse (get "dependents")
        |> ArrayParser.parse (get "patches")
        |> ArrayParser.parse (get "paths")
        |> ArrayParser.parse (get "terms")
        |> ArrayParser.parse (get "type-index")
        |> ArrayParser.parse (get "type-mentions-index")
        |> ArrayParser.parse (get "types")
