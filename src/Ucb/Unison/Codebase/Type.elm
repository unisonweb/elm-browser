module Ucb.Unison.Codebase.Type exposing
    ( GetTypeError(..)
    , httpGetType
    )

import Bytes exposing (Bytes)
import Task exposing (Task)
import Ucb.Util.Http as Http
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
    -- TODO
    Task.fail (GetTypeError_Other "")
