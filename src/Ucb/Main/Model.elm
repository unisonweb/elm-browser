module Ucb.Main.Model exposing (..)

import Ucb.Unison.Codebase.Path exposing (..)
import Ucb.Util.Http as Http
import Unison.Codebase.Causal exposing (..)
import Unison.Hash exposing (Hash32)


type Error
    = Err_GetHeadHash GetHeadHashError
    | Err_GetRawCausal GetRawCausalError


type alias Model =
    -- The head hash
    { headHash : Maybe Hash32

    -- The head branch
    , head : Maybe RawCausal

    -- The errors we've seen.
    , errors : List Error
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
