module GitHub exposing
    ( Dirent
    , File
    , RepoContents(..)
    , getFile
    , getRepoContents
    )

import Array exposing (Array)
import Bytes exposing (Bytes)
import Bytes.Decode
import Json.Decode
import Json.Decode.Pipeline
import Task exposing (Task)
import Ucb.Util.Http as Http


{-| TODO(mitchell) docs
-}
getFile :
    { owner : String
    , repo : String
    , ref : String
    , path : String
    , decoder : Bytes.Decode.Decoder a
    }
    -> Task (Http.Error Bytes) (Http.Response a)
getFile { owner, repo, ref, path, decoder } =
    Http.getBytes
        { decoder = decoder
        , headers = []
        , timeout = Nothing
        , url =
            String.join
                "/"
                [ "https://raw.githubusercontent.com"
                , owner
                , repo
                , ref
                , path
                ]
        }


{-| <https://developer.github.com/v3/repos/contents/#get-contents>

    Get repository "contents" (file or directory), given an owner, repo, and
    relative path.

-}
getRepoContents :
    String
    -> String
    -> String
    -> Task (Http.Error String) (Http.Response RepoContents)
getRepoContents owner repo path =
    Http.getJson
        { decoder = repoContentsDecoder
        , headers = []
        , timeout = Nothing
        , url = "https://api.github.com/repos/" ++ owner ++ "/" ++ repo ++ "/contents/" ++ path
        }


{-| Repo contents: a file or a directory.
-}
type RepoContents
    = FileContents File
    | DirectoryContents (Array Dirent)


repoContentsDecoder : Json.Decode.Decoder RepoContents
repoContentsDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map FileContents fileDecoder
        , Json.Decode.map DirectoryContents (Json.Decode.array direntDecoder)
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


fileDecoder : Json.Decode.Decoder File
fileDecoder =
    Json.Decode.succeed File
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "download_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "encoding" Json.Decode.string
        |> Json.Decode.Pipeline.required "git_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "html_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "_links" linksDecoder
        |> Json.Decode.Pipeline.required "path" Json.Decode.string
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "sha" Json.Decode.string
        |> Json.Decode.Pipeline.required "size" Json.Decode.int
        |> Json.Decode.Pipeline.required "type" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


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


direntDecoder : Json.Decode.Decoder Dirent
direntDecoder =
    Json.Decode.succeed Dirent
        |> Json.Decode.Pipeline.required "download_url" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "git_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "html_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "_links" linksDecoder
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "path" Json.Decode.string
        |> Json.Decode.Pipeline.required "sha" Json.Decode.string
        |> Json.Decode.Pipeline.required "size" Json.Decode.int
        |> Json.Decode.Pipeline.required "type" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string


linksDecoder : Json.Decode.Decoder { self : String, git : String, html : String }
linksDecoder =
    Json.Decode.succeed (\self git html -> { self = self, git = git, html = html })
        |> Json.Decode.Pipeline.required "self" Json.Decode.string
        |> Json.Decode.Pipeline.required "git" Json.Decode.string
        |> Json.Decode.Pipeline.required "html" Json.Decode.string
