module Ucb.GitHub exposing
    ( Dirent
    , File
    , RepoContents(..)
    , getRepoContents
    )

import Array exposing (Array)
import Http
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode


{-| <https://developer.github.com/v3/repos/contents/#get-contents>

    Get repository "contents" (file or directory), given an owner, repo, and
    relative path.

-}
getRepoContents :
    String
    -> String
    -> String
    -> Cmd (Result Http.Error RepoContents)
getRepoContents owner repo path =
    Http.get
        { url = "https://api.github.com/repos/" ++ owner ++ "/" ++ repo ++ "/contents/" ++ path
        , expect = Http.expectJson identity repoContentsDecoder
        }


{-| Repo contents: a file or a directory.
-}
type RepoContents
    = FileContents File
    | DirectoryContents (Array Dirent)


repoContentsDecoder : Decoder RepoContents
repoContentsDecoder =
    Decode.oneOf
        [ Decode.map FileContents fileDecoder
        , Decode.map DirectoryContents (Decode.array direntDecoder)
        ]


{-| A file.
-}
type alias File =
    { content : String
    , download_url : String
    , encoding : String
    , git_url : String
    , html_url : String
    , links : { self : String, git : String, html : String }
    , path : String
    , name : String
    , sha : String
    , size : Int
    , type_ : String
    , url : String
    }


fileDecoder : Decoder File
fileDecoder =
    Decode.succeed File
        |> Decode.required "content" Decode.string
        |> Decode.required "download_url" Decode.string
        |> Decode.required "encoding" Decode.string
        |> Decode.required "git_url" Decode.string
        |> Decode.required "html_url" Decode.string
        |> Decode.required "_links" linksDecoder
        |> Decode.required "path" Decode.string
        |> Decode.required "name" Decode.string
        |> Decode.required "sha" Decode.string
        |> Decode.required "size" Decode.int
        |> Decode.required "type" Decode.string
        |> Decode.required "url" Decode.string


{-| A directory entry.
-}
type alias Dirent =
    { download_url : Maybe String
    , git_url : String
    , html_url : String
    , links : { self : String, git : String, html : String }
    , name : String
    , path : String
    , sha : String
    , size : Int
    , type_ : String
    , url : String
    }


direntDecoder : Decoder Dirent
direntDecoder =
    Decode.succeed Dirent
        |> Decode.required "download_url" (Decode.nullable Decode.string)
        |> Decode.required "git_url" Decode.string
        |> Decode.required "html_url" Decode.string
        |> Decode.required "_links" linksDecoder
        |> Decode.required "name" Decode.string
        |> Decode.required "path" Decode.string
        |> Decode.required "sha" Decode.string
        |> Decode.required "size" Decode.int
        |> Decode.required "type" Decode.string
        |> Decode.required "url" Decode.string


linksDecoder : Decoder { self : String, git : String, html : String }
linksDecoder =
    Decode.succeed (\self git html -> { self = self, git = git, html = html })
        |> Decode.required "self" Decode.string
        |> Decode.required "git" Decode.string
        |> Decode.required "html" Decode.string
