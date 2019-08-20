module Ucb.Unison.Codebase.Type exposing
    ( GetTypeError(..)
    , httpGetType
    )

import Bytes exposing (Bytes)
import Task exposing (Task)
import Ucb.GitHub
import Ucb.Util.Http as Http
import Unison.Codebase.Serialization.V1 as V1
import Unison.Reference exposing (..)
import Unison.Symbol exposing (..)
import Unison.Type exposing (..)


{-| Something went wrong when getting a type.
-}
type GetTypeError
    = GetTypeError_Http (Http.Error Bytes)
    | GetTypeError_Other String


httpGetType :
    String
    -> String
    -> Id
    -> Task GetTypeError ( Id, Http.Response (Type Symbol) )
httpGetType owner repo id =
    Ucb.GitHub.httpGetFile
        owner
        repo
        -- TODO probably want to get default branch somehow?
        "master"
        (".unison/v1/types/" ++ idToPath id)
        V1.typeDecoder
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
