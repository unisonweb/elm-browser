module Ucb.Unison.Codebase.API exposing
    ( UnisonCodebaseAPI
    , getBranch
    , getTermTypes
    , getTypeDecls
    )

import Bytes exposing (Bytes)
import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Misc exposing (..)
import Task exposing (Task)
import Ucb.Unison.BranchDict exposing (..)
import Ucb.Util.Http as Http
import Unison.Codebase.Branch exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Declaration exposing (..)
import Unison.Hash exposing (..)
import Unison.Reference exposing (..)
import Unison.Symbol exposing (..)
import Unison.Term exposing (..)
import Unison.Type exposing (..)
import Util.HashSet as HashSet


{-| An abstract interfact to the Unison codebase served over HTTP.
-}
type alias UnisonCodebaseAPI =
    { -- Get the head namespace hash, that is, the name of the file located at
      -- .unison/v1/paths/\_head/<namespace-hash>
      getHeadHash : Task (Http.Error String) (Http.Response BranchHash)
    , getRawCausal : BranchHash -> Task (Http.Error Bytes) ( BranchHash, Http.Response (RawCausal RawBranch) )
    , getTerm : Id -> Task (Http.Error Bytes) ( Id, Http.Response (Term Symbol) )
    , getTermType : Id -> Task (Http.Error Bytes) ( Id, Http.Response (Type Symbol) )
    , getTypeDecl : Id -> Task (Http.Error Bytes) ( Id, Http.Response (Declaration Symbol) )
    }


{-| Given a branch hash, return a map full of branches keyed by their
hashes.

Postcondition: the map contains an entry for every descendant of the given hash
(and the hash itself). That is, it fetches the whole family tree.

-}
getBranch :
    UnisonCodebaseAPI
    ->
        { r
            | branches : BranchDict Branch
            , parents : BranchDict (HashSet BranchHash)
            , successors : BranchDict (HashSet BranchHash)
        }
    -> BranchHash
    ->
        Task (Http.Error Bytes)
            ( BranchHash
            , { branches : BranchDict Branch
              , parents : BranchDict (HashSet BranchHash)
              , successors : BranchDict (HashSet BranchHash)
              }
            )
getBranch api cache hash =
    getBranch2 api cache hash
        |> Task.map (\newCache -> ( hash, newCache ))


getBranch2 :
    UnisonCodebaseAPI
    ->
        { r
            | branches : BranchDict Branch
            , parents : BranchDict (HashSet BranchHash)
            , successors : BranchDict (HashSet BranchHash)
        }
    -> BranchHash
    ->
        Task (Http.Error Bytes)
            { branches : BranchDict Branch
            , parents : BranchDict (HashSet BranchHash)
            , successors : BranchDict (HashSet BranchHash)
            }
getBranch2 api cache hash =
    case HashDict.get hash cache.branches of
        Nothing ->
            api.getRawCausal hash
                |> Task.andThen (getBranch3 api cache)

        Just branch ->
            Task.succeed
                { branches = cache.branches
                , parents = cache.parents
                , successors = cache.successors
                }


getBranch3 :
    UnisonCodebaseAPI
    ->
        { r
            | branches : BranchDict Branch
            , parents : BranchDict (HashSet BranchHash)
            , successors : BranchDict (HashSet BranchHash)
        }
    -> ( BranchHash, Http.Response (RawCausal RawBranch) )
    ->
        Task (Http.Error Bytes)
            { branches : BranchDict Branch
            , parents : BranchDict (HashSet BranchHash)
            , successors : BranchDict (HashSet BranchHash)
            }
getBranch3 api cache ( hash, { body } ) =
    let
        children : List BranchHash
        children =
            rawCausalChildren body

        -- Might as well update the parents/successors
        -- before making the recursive call, though it is
        -- not necessary to.
        newCache :
            { branches : BranchDict Branch
            , parents : BranchDict (HashSet BranchHash)
            , successors : BranchDict (HashSet BranchHash)
            }
        newCache =
            { branches = cache.branches
            , parents =
                insertParents
                    hash
                    children
                    cache.parents
            , successors =
                insertSuccessors
                    hash
                    (rawCausalPredecessors body)
                    cache.successors
            }
    in
    -- First, recursively call 'getBranch2' on each child.
    -- By the type it returns, the map will contain an entry
    -- for every one of our descendants.
    tasks
        children
        (getBranch2 api)
        newCache
        -- Finally, add ourselves to the map, by repeatedly
        -- reaching into it to convert our
        -- 'RawCausal RawBranch' into a 'RawCausal Branch0'.
        |> Task.map (getBranch4 hash body)


{-| Precondition: the given cache has all the info we need (our descendants).
-}
getBranch4 :
    BranchHash
    -> RawCausal RawBranch
    ->
        { r
            | branches : BranchDict Branch
        }
    ->
        { r
            | branches : BranchDict Branch
        }
getBranch4 hash causal cache =
    { cache
        | branches =
            HashDict.insert
                hash
                (Branch
                    (rawCausalMap
                        (\_ ->
                            rawBranchToBranch0
                                (\hash_ ->
                                    case HashDict.get hash_ cache.branches of
                                        Nothing ->
                                            impossible <|
                                                String.join "\n"
                                                    [ "rawBranchToBranch0: fatal error"
                                                    , ""
                                                    , "could not find branch with hash"
                                                    , ""
                                                    , "  " ++ hash_
                                                    , ""
                                                    , "in branch cache, which contains:"
                                                    , ""
                                                    , cache.branches
                                                        |> HashDict.toList
                                                        |> List.map Tuple.first
                                                        |> List.map (\s -> "  " ++ s)
                                                        |> String.join "\n"
                                                    ]

                                        Just branch ->
                                            branch
                                )
                                (rawCausalHead causal)
                        )
                        causal
                    )
                )
                cache.branches
    }


{-| Given a branch and its children, insert the branch as a parent of each of
its children.
-}
insertParents :
    BranchHash
    -> List BranchHash
    -> BranchDict (HashSet BranchHash)
    -> BranchDict (HashSet BranchHash)
insertParents parent children parentsCache =
    List.foldl
        (\child ->
            HashDict.update
                child
                (\existingParents ->
                    case existingParents of
                        Nothing ->
                            Just
                                (HashSet.singleton
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
    -> BranchDict (HashSet BranchHash)
    -> BranchDict (HashSet BranchHash)
insertSuccessors successor predecessors successorsCache =
    List.foldl
        (\predecessor ->
            HashDict.update
                predecessor
                (\existingSuccessors ->
                    case existingSuccessors of
                        Nothing ->
                            Just
                                (HashSet.singleton
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


{-| Run a task with every 'a' in the given list, statefully modifying an 's'.
-}
tasks :
    List a
    -> (s -> a -> Task error s)
    -> (s -> Task error s)
tasks xs f s0 =
    case xs of
        [] ->
            Task.succeed s0

        y :: ys ->
            f s0 y
                |> Task.andThen (tasks ys f)


{-| Batch getTermType
-}
getTermTypes :
    UnisonCodebaseAPI
    -> List Id
    -> Task (Http.Error Bytes) (List ( Id, Type Symbol ))
getTermTypes api ids =
    case ids of
        [] ->
            Task.succeed []

        id :: ids_ ->
            Task.map2
                (\( r, s ) rs -> ( r, s.body ) :: rs)
                (api.getTermType id)
                (getTermTypes api ids_)


{-| Batch getTypeDecl
-}
getTypeDecls :
    UnisonCodebaseAPI
    -> List Id
    -> Task (Http.Error Bytes) (List ( Id, Declaration Symbol ))
getTypeDecls api ids =
    case ids of
        [] ->
            Task.succeed []

        id :: ids_ ->
            Task.map2
                (\( r, s ) rs -> ( r, s.body ) :: rs)
                (api.getTypeDecl id)
                (getTypeDecls api ids_)
