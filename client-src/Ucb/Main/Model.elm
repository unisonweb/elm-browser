module Ucb.Main.Model exposing (..)

import Bytes exposing (Bytes)
import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Misc exposing (hashSetSingleton)
import Ucb.Unison.Codebase.API exposing (..)
import Ucb.Util.Http as Http
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Declaration exposing (..)
import Unison.Hash exposing (..)
import Unison.Reference exposing (..)
import Unison.Referent exposing (..)
import Unison.Symbol exposing (..)
import Unison.Term exposing (..)
import Unison.Type exposing (..)


type Error
    = Err_GetHeadHash (Http.Error String)
    | Err_GetBranch (Http.Error Bytes)
    | Err_GetRawCausal (Http.Error Bytes)
    | Err_GetTerm (Http.Error Bytes)
    | Err_GetTermType (Http.Error Bytes)
    | Err_GetType (Http.Error Bytes)


type alias Model =
    { -- APIs that can be swapped out or mocked.
      api :
        { unison : UnisonCodebaseAPI
        }

    -- The codebase
    , codebase :
        { -- This data we've fetched directly from the codebase
          head : Maybe BranchHash
        , branches : HashDict BranchHash (RawCausal RawBranch)
        , branches2 : HashDict BranchHash Branch -- TODO replace 'branches' with this
        , terms : HashDict Referent (Term Symbol)
        , termTypes : HashDict Referent (Type Symbol)
        , types : HashDict Reference (Declaration Symbol)

        -- Mapping from branch to its parent(s). The codebase doesn't provide
        -- this, we just discover and cache it lazily as you move down into
        -- children.
        , parents : HashDict BranchHash (HashSet BranchHash)

        -- Mapping from branch to its successor(s). The codebase doesn't
        -- provide this, we just discover and cache it lazily as you move
        -- backwards in time.
        , successors : HashDict BranchHash (HashSet BranchHash)
        }

    -- UI state, not pulled (nor derived) from the codebase
    , ui :
        -- Visible?
        { branches : HashDict BranchHash Bool
        , terms : HashDict Referent Bool
        , types : HashDict Reference Bool
        }

    -- The errors we've seen. Just slappin' them in the model to put into the
    -- HTML when something is going wrong.
    , errors : List Error

    -- GitHub rate limit (again just for debugging purposes)
    , rateLimit : Maybe String
    }


{-| Given a branch and its children, insert the branch as a parent of each of
its children.
-}
insertParents :
    BranchHash
    -> List BranchHash
    -> HashDict BranchHash (HashSet BranchHash)
    -> HashDict BranchHash (HashSet BranchHash)
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
    BranchHash
    -> List BranchHash
    -> HashDict BranchHash (HashSet BranchHash)
    -> HashDict BranchHash (HashSet BranchHash)
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
