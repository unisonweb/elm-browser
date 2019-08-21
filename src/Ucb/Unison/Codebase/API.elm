module Ucb.Unison.Codebase.API exposing (..)

import Bytes exposing (Bytes)
import Task exposing (Task)
import Ucb.Util.Http as Http
import Unison.Codebase.Causal exposing (..)
import Unison.Declaration exposing (..)
import Unison.Hash exposing (Hash32)
import Unison.Reference exposing (..)
import Unison.Symbol exposing (..)


{-| An abstract interfact to the Unison codebase served over HTTP.

It would be nice to parameterize this over a type variable or two to allow
different backends (like GitHub vs. some other file server) to vary a bit.
However, I suspect that will cause abstraction pain elsewhere due to Elm's lack
of existential types.

So, I opted for the "lowest common denominator" interface, with some warts
(like using a simple String to capture "other errors"), but overall it should
make reusing code simpler.

-}
type alias UnisonCodebaseAPI =
    { -- Get the head namespace hash, that is, the name of the file located at
      -- .unison/v1/paths/\_head/<namespace-hash>
      getHeadHash : Task GetHeadHashError (Http.Response Hash32)
    , getRawCausal : Hash32 -> Task GetRawCausalError ( Hash32, Http.Response RawCausal )
    , getType : Id -> Task GetTypeError ( Id, Http.Response (Declaration Symbol) )
    }


{-| Something went wrong when getting the head hash.
-}
type GetHeadHashError
    = GetHeadHashError_Http (Http.Error String)
    | GetHeadHashError_Other String


{-| Something went wrong when getting a raw causal.
-}
type GetRawCausalError
    = GetRawCausalError_Http (Http.Error Bytes)
    | GetRawCausalError_Other String


{-| Something went wrong when getting a type.
-}
type GetTypeError
    = GetTypeError_Http (Http.Error Bytes)
    | GetTypeError_Other String
