module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Bytes exposing (Bytes)
import Dict
import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Misc exposing (..)
import Task exposing (Task)
import Ucb.Main.Message exposing (Message(..))
import Ucb.Main.Model exposing (..)
import Ucb.Main.View exposing (view)
import Ucb.Unison.BranchDict as BranchDict exposing (BranchDict)
import Ucb.Unison.Codebase.API exposing (..)
import Ucb.Unison.Codebase.API.GitHub exposing (..)
import Ucb.Unison.Codebase.API.LocalServer exposing (..)
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
import Url
import Util.HashSet as HashSet


main : Program () Model Message
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


{-| Port that elm-live runs on
-}
elmLivePort : number
elmLivePort =
    8000


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Message )
init _ url key =
    let
        isDevMode : Bool
        isDevMode =
            case url.port_ of
                Nothing ->
                    False

                Just portNum ->
                    portNum == elmLivePort

        model : Model
        model =
            { api =
                { unison = makeLocalServerUnisonCodebaseAPI isDevMode
                }
            , codebase =
                { head = Nothing
                , branches = BranchDict.empty
                , terms = HashDict.empty idEquality idHashing
                , termTypes = HashDict.empty idEquality idHashing
                , typeDecls = HashDict.empty idEquality idHashing
                , parents = BranchDict.empty
                , successors = BranchDict.empty
                }
            , ui =
                { branches = BranchDict.empty
                , terms = HashDict.empty idEquality idHashing
                , search = ""
                , key = key
                }
            , errors = []
            , isDevMode = isDevMode
            }

        -- First command: fetch _head path!
        initialCommand : Cmd Message
        initialCommand =
            model.api.unison.getHeadHash
                |> Task.attempt Http_GetHeadHash
    in
    ( model, initialCommand )


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Http_GetHeadHash result ->
            update_Http_GetHeadHash result model

        Http_GetBranch result ->
            update_Http_GetBranch result model

        Http_GetTerm result ->
            update_Http_GetTerm result model

        Http_GetTermTypesAndTypeDecls result ->
            update_Http_GetTermTypesAndTypeDecls result model

        User_FocusBranch hash ->
            update_User_FocusBranch hash model

        User_Search search ->
            update_User_Search search model

        User_ToggleBranch hash ->
            update_User_ToggleBranch hash model

        User_ToggleTerm id ->
            update_User_ToggleTerm id model

        User_DebugButton ->
            update_User_DebugButton model

        UrlChanged _ ->
            ( model, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )


{-| Whatever you're debugging. Might be nothing!
-}
update_User_DebugButton :
    Model
    -> ( Model, Cmd Message )
update_User_DebugButton model =
    ( model, Cmd.none )


{-| Got the head hash. Next step: get the actual (decoded) bytes.
-}
update_Http_GetHeadHash :
    Result (Http.Error String) (Http.Response BranchHash)
    -> Model
    -> ( Model, Cmd Message )
update_Http_GetHeadHash result model =
    case result of
        Err err ->
            ( { model | errors = Err_GetHeadHash err :: model.errors }
            , Cmd.none
            )

        Ok response ->
            update_Http_GetHeadHash2 response model


update_Http_GetHeadHash2 :
    Http.Response BranchHash
    -> Model
    -> ( Model, Cmd Message )
update_Http_GetHeadHash2 response model =
    ( model
    , getBranch
        model.api.unison
        model.codebase
        response.body
        |> Task.attempt Http_GetBranch
    )


update_Http_GetBranch :
    Result (Http.Error Bytes)
        ( BranchHash
        , { branches : BranchDict Branch
          , parents : BranchDict (HashSet BranchHash)
          , successors : BranchDict (HashSet BranchHash)
          }
        )
    -> Model
    -> ( Model, Cmd Message )
update_Http_GetBranch result model =
    case result of
        Err err ->
            ( { model | errors = Err_GetBranch err :: model.errors }
            , Cmd.none
            )

        Ok result2 ->
            update_Http_GetBranch2 result2 model


update_Http_GetBranch2 :
    ( BranchHash
    , { branches : HashDict BranchHash Branch
      , parents : HashDict BranchHash (HashSet BranchHash)
      , successors : HashDict BranchHash (HashSet BranchHash)
      }
    )
    -> Model
    -> ( Model, Cmd Message )
update_Http_GetBranch2 ( hash, { branches, parents, successors } ) model =
    let
        newBranches : BranchDict Branch
        newBranches =
            HashDict.foldl
                (\( hash_, branch ) ->
                    HashDict.update
                        hash_
                        (maybe (Just branch) Just)
                )
                model.codebase.branches
                branches
    in
    ( { model
        | codebase =
            { -- We assume that because we fetched this branch, we wanted to
              -- focus it.
              head = Just hash
            , branches = newBranches
            , parents =
                (BranchDict.monoid HashSet.semigroup).semigroup.prepend
                    parents
                    model.codebase.parents
            , successors =
                (BranchDict.monoid HashSet.semigroup).semigroup.prepend
                    successors
                    model.codebase.successors
            , terms = model.codebase.terms
            , termTypes = model.codebase.termTypes
            , typeDecls = model.codebase.typeDecls
            }
      }
    , case HashDict.get hash newBranches of
        -- This should never be the case
        Nothing ->
            Cmd.none

        Just branch ->
            Task.map2
                Tuple.pair
                (getMissingTermTypes model branch)
                (getMissingTypeDecls model branch)
                |> Task.attempt Http_GetTermTypesAndTypeDecls
    )


update_Http_GetTerm :
    Result (Http.Error Bytes) ( Id, Http.Response (Term Symbol) )
    -> Model
    -> ( Model, Cmd message )
update_Http_GetTerm result model =
    case result of
        Err err ->
            ( { model | errors = Err_GetTerm err :: model.errors }
            , Cmd.none
            )

        Ok response ->
            update_Http_GetTerm2 response model


update_Http_GetTerm2 :
    ( Id, Http.Response (Term Symbol) )
    -> Model
    -> ( Model, Cmd message )
update_Http_GetTerm2 ( id, response ) model =
    ( { model
        | codebase =
            { terms = HashDict.insert id response.body model.codebase.terms

            -- unchanged
            , head = model.codebase.head
            , branches = model.codebase.branches
            , termTypes = model.codebase.termTypes
            , typeDecls = model.codebase.typeDecls
            , parents = model.codebase.parents
            , successors = model.codebase.successors
            }
      }
    , Cmd.none
    )


update_Http_GetTermTypesAndTypeDecls :
    Result (Http.Error Bytes) ( List ( Id, Type Symbol ), List ( Id, Declaration Symbol ) )
    -> Model
    -> ( Model, Cmd message )
update_Http_GetTermTypesAndTypeDecls result model =
    case result of
        Err err ->
            ( { model | errors = Err_GetTypeDecls err :: model.errors }
            , Cmd.none
            )

        Ok response ->
            update_Http_GetTermTypesAndTypeDecls2 response model


update_Http_GetTermTypesAndTypeDecls2 :
    ( List ( Id, Type Symbol ), List ( Id, Declaration Symbol ) )
    -> Model
    -> ( Model, Cmd message )
update_Http_GetTermTypesAndTypeDecls2 ( termTypes, types ) model =
    ( { model
        | codebase =
            { termTypes =
                List.foldl
                    (\( id, type_ ) -> HashDict.insert id type_)
                    model.codebase.termTypes
                    termTypes
            , typeDecls =
                List.foldl
                    (\( id, declaration ) -> HashDict.insert id declaration)
                    model.codebase.typeDecls
                    types

            -- unchanged
            , head = model.codebase.head
            , branches = model.codebase.branches
            , terms = model.codebase.terms
            , parents = model.codebase.parents
            , successors = model.codebase.successors
            }
      }
    , Cmd.none
    )


{-| The user has adjusted the current search.
-}
update_User_Search :
    String
    -> Model
    -> ( Model, Cmd msg )
update_User_Search search model =
    ( { model
        | ui =
            { search = String.toLower search

            -- unchanged
            , branches = model.ui.branches
            , terms = model.ui.terms
            , key = model.ui.key
            }
      }
    , Cmd.none
    )


{-| Focus a branch:

  - We might've already fetched it (e.g. it's the child of a previous root). In
    this case we have no branches to fetch, but we do want to fetch all of the
    new branch's types and terms (currently: just types, terms is a WIP).

  - Otherwise, it's somewhere in our history. So fetch it and all of its
    children! When the request comes back, we'll switch focus.

-}
update_User_FocusBranch :
    BranchHash
    -> Model
    -> ( Model, Cmd Message )
update_User_FocusBranch hash model =
    case HashDict.get hash model.codebase.branches of
        Nothing ->
            ( model
            , getBranch
                model.api.unison
                model.codebase
                hash
                |> Task.attempt Http_GetBranch
            )

        Just branch ->
            ( { model
                | codebase =
                    { head = Just hash

                    -- unchanged
                    , branches = model.codebase.branches
                    , terms = model.codebase.terms
                    , termTypes = model.codebase.termTypes
                    , typeDecls = model.codebase.typeDecls
                    , parents = model.codebase.parents
                    , successors = model.codebase.successors
                    }
              }
            , Task.map2
                Tuple.pair
                (getMissingTermTypes model branch)
                (getMissingTypeDecls model branch)
                |> Task.attempt Http_GetTermTypesAndTypeDecls
            )


update_User_ToggleBranch :
    BranchHash
    -> Model
    -> ( Model, Cmd Message )
update_User_ToggleBranch hash model =
    ( { model
        | ui =
            { branches =
                HashDict.update
                    hash
                    (maybe True not >> Just)
                    model.ui.branches

            -- unchanged
            , search = model.ui.search
            , terms = model.ui.terms
            , key = model.ui.key
            }
      }
    , case HashDict.get hash model.codebase.branches of
        -- Should never be the case
        Nothing ->
            Cmd.none

        Just branch ->
            Task.map2
                Tuple.pair
                (getMissingTermTypes model branch)
                (getMissingTypeDecls model branch)
                |> Task.attempt Http_GetTermTypesAndTypeDecls
    )


update_User_ToggleTerm :
    Id
    -> Model
    -> ( Model, Cmd Message )
update_User_ToggleTerm id model =
    let
        command : Cmd Message
        command =
            case HashDict.get id model.codebase.terms of
                Nothing ->
                    model.api.unison.getTerm id
                        |> Task.attempt Http_GetTerm

                Just _ ->
                    Cmd.none

        newTerms : HashDict Id Bool
        newTerms =
            HashDict.update
                id
                (maybe True not >> Just)
                model.ui.terms
    in
    ( { model
        | ui =
            { terms = newTerms

            -- unchanged
            , branches = model.ui.branches
            , search = model.ui.search
            , key = model.ui.key
            }
      }
    , command
    )


subscriptions :
    Model
    -> Sub Message
subscriptions _ =
    Sub.none
