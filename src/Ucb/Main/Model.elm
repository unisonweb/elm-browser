module Ucb.Main.Model exposing (..)

import HashingContainers.HashDict as HashDict exposing (HashDict)
import Ucb.Unison.Codebase.Path exposing (..)
import Ucb.Util.Http as Http
import Unison.Codebase.Causal exposing (..)
import Unison.Hash exposing (Hash32)


type Error
    = Err_GetHeadHash GetHeadHashError
    | Err_GetRawCausal GetRawCausalError


type alias Model =
    -- The current head
    { head : Maybe Hash32

    -- The codebase
    , codebase :
        { branches : HashDict Hash32 RawCausal

        -- Mapping from branch to future branch. Don't think the codebase
        -- provides this, we just discover and cache it lazily as you move
        -- backwards in time.
        , next : HashDict Hash32 Hash32
        }

    -- UI state
    , ui :
        -- Visible?
        { branches : HashDict Hash32 Bool
        }

    -- The errors we've seen.
    , errors : List Error

    -- GitHub rate limit
    , rateLimit : Maybe String
    }


{-| Accumulate an error in the model.
-}
accumulateError :
    Error
    -> Model
    -> Model
accumulateError err model =
    { model
        | errors = err :: model.errors
    }
