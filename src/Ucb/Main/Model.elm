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
        { -- This data we've fetched directly from the codebase
          branches : HashDict Hash32 RawCausal
        , types : HashDict Id (Type Symbol)

        -- Mapping from branch to its parent(s). The codebase doesn't provide
        -- this, we just discover and cache it lazily as you move down into
        -- children.
        , parents : HashDict Hash32 (HashSet Hash32)

        -- Mapping from branch to its successor(s). The codebase doesn't
        -- provide this, we just discover and cache it lazily as you move
        -- backwards in time.
        , successors : HashDict Hash32 (HashSet Hash32)
        }

    -- UI state, not pulled (nor derived) from the codebase
    , ui :
        -- Visible?
        { branches : HashDict Hash32 Bool
        }

    -- The errors we've seen. Just slappin' them in the model to put into the
    -- HTML when something is going wrong.
    , errors : List Error

    -- GitHub rate limit (again just for debugging purposes)
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


{-| Given a branch and its children, insert the branch as a parent of each of
its children.
-}
insertParents :
    Hash32
    -> List Hash32
    -> HashDict Hash32 (HashSet Hash32)
    -> HashDict Hash32 (HashSet Hash32)
insertParents parent children parentsCache =
    List.foldl
        (\child ->
            HashDict.update
                child
                (\existingParents ->
                    case existingParents of
                        Nothing ->
                            Just
                                (hashSetSingleton
                                    hash32Equality
                                    hash32Hashing
                                    parent
                                )

                        Just existingParents_ ->
                            Just (HashSet.insert parent existingParents_)
                )
        )
        parentsCache
        children


{-| Given a branch and its predecessors, insert the branch as a successor of
each of its predecessors.
-}
insertSuccessors :
    Hash32
    -> List Hash32
    -> HashDict Hash32 (HashSet Hash32)
    -> HashDict Hash32 (HashSet Hash32)
insertSuccessors successor predecessors successorsCache =
    List.foldl
        (\predecessor ->
            HashDict.update
                predecessor
                (\existingSuccessors ->
                    case existingSuccessors of
                        Nothing ->
                            Just
                                (hashSetSingleton
                                    hash32Equality
                                    hash32Hashing
                                    successor
                                )

                        Just existingSuccessors_ ->
                            Just (HashSet.insert successor existingSuccessors_)
                )
        )
        successorsCache
        predecessors
