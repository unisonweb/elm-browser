module Ucb.Main.Model exposing (..)

import Browser.Navigation as Nav
import Bytes exposing (Bytes)
import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Task exposing (Task)
import Ucb.Unison.BranchDict exposing (..)
import Ucb.Unison.Codebase.API exposing (..)
import Ucb.Util.Http as Http
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Codebase.NameSegment exposing (..)
import Unison.Codebase.Patch exposing (..)
import Unison.Declaration exposing (..)
import Unison.Hash exposing (..)
import Unison.Reference exposing (..)
import Unison.Referent exposing (..)
import Unison.Symbol exposing (..)
import Unison.Term exposing (..)
import Unison.Type exposing (..)
import Util.HashSet as HashSet


type Error
    = Err_GetHeadHash (Http.Error String)
    | Err_GetBranch (Http.Error Bytes)
    | Err_GetPatches (Http.Error Bytes)
    | Err_GetRawCausal (Http.Error Bytes)
    | Err_GetTerm (Http.Error Bytes)
    | Err_GetTermTypes (Http.Error Bytes)
    | Err_GetTypeDecl (Http.Error Bytes)
    | Err_GetTypeDecls (Http.Error Bytes)


type alias Model =
    { -- APIs that can be swapped out or mocked.
      api :
        { unison : UnisonCodebaseAPI
        }

    -- The codebase
    , codebase : ModelCodebase

    -- UI state
    , ui : ModelUI

    -- The errors we've seen. Just slappin' them in the model to put into the
    -- HTML when something is going wrong.
    , errors : List Error

    -- Whether we're running in dev and need to use CORS headers
    , isDevMode : Bool
    }


type alias ModelCodebase =
    { -- This data we've fetched directly from the codebase
      head : Maybe BranchHash
    , branches : BranchDict Branch
    , patches : HashDict PatchHash Patch
    , terms : HashDict Id (Term Symbol)
    , termTypes : HashDict Id (Type Symbol)
    , typeDecls : HashDict Id (Declaration Symbol)

    -- Mapping from branch to its parent(s). The codebase doesn't provide
    -- this, we just discover and cache it lazily as you move down into
    -- children.
    , parents : BranchDict (HashSet BranchHash)

    -- Mapping from branch to its successor(s). The codebase doesn't
    -- provide this, we just discover and cache it lazily as you move
    -- backwards in time.
    , successors : BranchDict (HashSet BranchHash)
    }


type alias ModelUI =
    { -- Branch path we're currently viewing
      branch : List NameSegment

    -- Visible?
    , terms : HashDict Id Bool

    -- Search box
    , search : String

    -- hover tracking
    , hoveredTerm : Maybe Id
    , key : Nav.Key
    }


{-| Given a branch, fetch all of its patches that we haven't already.
-}
getMissingPatches :
    Model
    -> Branch
    -> Task (Http.Error Bytes) (List ( PatchHash, Patch ))
getMissingPatches model branch =
    getPatches
        model.api.unison
        (branch
            |> branchPatchHashes
            |> HashSet.toList
            |> List.filterMap
                (\hash ->
                    case HashDict.get hash model.codebase.patches of
                        Nothing ->
                            Just hash

                        Just _ ->
                            Nothing
                )
        )


{-| Given a branch we just switched to, fetch all of the (shallow) terms' types
that we haven't already.
-}
getMissingTermTypes :
    Model
    -> Branch
    -> Task (Http.Error Bytes) (List ( Id, Type Symbol ))
getMissingTermTypes model branch =
    getTermTypes
        model.api.unison
        (List.filterMap
            (\referent ->
                case referent of
                    Con _ _ _ ->
                        Nothing

                    Ref (Builtin _) ->
                        Nothing

                    Ref (Derived id) ->
                        case HashDict.get id model.codebase.termTypes of
                            Nothing ->
                                Just id

                            _ ->
                                Nothing
            )
            (HashSet.toList (branchHead branch).terms.fact)
        )


{-| Given a branch we just switched to, fetch all of the (shallow) type
declarations that we haven't already.
-}
getMissingTypeDecls :
    Model
    -> Branch
    -> Task (Http.Error Bytes) (List ( Id, Declaration Symbol ))
getMissingTypeDecls model branch =
    getTypeDecls
        model.api.unison
        (List.filterMap
            (\reference ->
                case reference of
                    Builtin _ ->
                        Nothing

                    Derived id ->
                        case HashDict.get id model.codebase.typeDecls of
                            Nothing ->
                                Just id

                            _ ->
                                Nothing
            )
            (HashSet.toList (branchHead branch).types.fact)
        )
