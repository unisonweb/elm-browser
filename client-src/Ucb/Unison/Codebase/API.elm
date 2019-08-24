module Ucb.Unison.Codebase.API exposing (..)

import Bytes exposing (Bytes)
import Task exposing (Task)
import Ucb.Util.Http as Http
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Declaration exposing (..)
import Unison.Hash exposing (Hash32)
import Unison.Reference exposing (..)
import Unison.Symbol exposing (..)
import Unison.Term exposing (..)
import Unison.Type exposing (..)


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
      getHeadHash : Task (Http.Error String) (Http.Response Hash32)
    , getRawCausal : Hash32 -> Task (Http.Error Bytes) ( Hash32, Http.Response (RawCausal RawBranch) )
    , getTerm : Id -> Task (Http.Error Bytes) ( Id, Http.Response (Term Symbol) )
    , getTermType : Id -> Task (Http.Error Bytes) ( Id, Http.Response (Type Symbol) )
    , getType : Id -> Task (Http.Error Bytes) ( Id, Http.Response (Declaration Symbol) )
    }
