module Ucb.Main.Model exposing (..)

import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Misc exposing (hashSetSingleton)
import Ucb.Unison.Codebase.Path exposing (..)
import Ucb.Unison.Codebase.Type exposing (..)
import Ucb.Util.Http as Http
import Unison.Codebase.Causal exposing (..)
import Unison.Hash exposing (..)
import Unison.Reference exposing (..)
import Unison.Symbol exposing (..)
import Unison.Type exposing (..)


type Error
    = Err_GetHeadHash GetHeadHashError
    | Err_GetRawCausal GetRawCausalError
    | Err_GetType GetTypeError


type alias Model =
    -- The current head
    { head : Maybe Hash32

    -- The codebase
    , codebase :
        { branches : HashDict Hash32 RawCausal

        -- Mapping from branch to its successor(s). Don't think the codebase
        -- provides this, we just discover and cache it lazily as you move
        -- backwards in time.
        , successors : HashDict Hash32 (HashSet Hash32)
        , types : HashDict Id (Type Symbol)
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


{-| Given a branch and its predecessors, insert the branch as a successor of
each of its predecessors.
-}
insertSuccessors :
    Hash32
    -> List Hash32
    -> HashDict Hash32 (HashSet Hash32)
    -> HashDict Hash32 (HashSet Hash32)
insertSuccessors hash preds successors =
    List.foldl
        (\pred ->
            HashDict.update
                pred
                (\existingSuccessors ->
                    case existingSuccessors of
                        Nothing ->
                            Just
                                (hashSetSingleton
                                    hash32Equality
                                    hash32Hashing
                                    hash
                                )

                        Just existingSuccessors_ ->
                            Just (HashSet.insert hash existingSuccessors_)
                )
        )
        successors
        preds
