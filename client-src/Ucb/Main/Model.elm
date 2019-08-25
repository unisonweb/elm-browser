module Ucb.Main.Model exposing (..)

import Bytes exposing (Bytes)
import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Misc exposing (hashSetSingleton)
import Task exposing (Task)
import Ucb.Unison.BranchDict exposing (..)
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
    | Err_GetTypes (Http.Error Bytes)


type alias Model =
    { -- APIs that can be swapped out or mocked.
      api :
        { unison : UnisonCodebaseAPI
        }

    -- The codebase
    , codebase :
        { -- This data we've fetched directly from the codebase
          head : Maybe BranchHash
        , branches : BranchDict Branch
        , terms : HashDict Referent (Term Symbol)
        , termTypes : HashDict Referent (Type Symbol)
        , types : HashDict Reference (Declaration Symbol)

        -- Mapping from branch to its parent(s). The codebase doesn't provide
        -- this, we just discover and cache it lazily as you move down into
        -- children.
        , parents : BranchDict (HashSet BranchHash)

        -- Mapping from branch to its successor(s). The codebase doesn't
        -- provide this, we just discover and cache it lazily as you move
        -- backwards in time.
        , successors : BranchDict (HashSet BranchHash)
        }

    -- UI state, not pulled (nor derived) from the codebase
    , ui :
        -- Visible?
        { branches : BranchDict Bool
        , terms : HashDict Referent Bool
        }

    -- The errors we've seen. Just slappin' them in the model to put into the
    -- HTML when something is going wrong.
    , errors : List Error
    }


{-| Given a branch we just switched to, fetch all of the type declarations that
we haven't already.
-}
getMissingTypes :
    Model
    -> Branch
    -> Task (Http.Error Bytes) (List ( Id, Declaration Symbol ))
getMissingTypes model (Branch causal) =
    getTypes
        model.api.unison
        (List.filterMap
            (\reference ->
                case HashDict.get reference model.codebase.types of
                    Nothing ->
                        case reference of
                            Derived id ->
                                Just id

                            Builtin _ ->
                                Nothing

                    _ ->
                        Nothing
            )
            (HashSet.toList (rawCausalHead causal).types.fact)
        )
