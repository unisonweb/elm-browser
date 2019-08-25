module Main exposing (..)

import Browser
import Bytes exposing (Bytes)
import Dict
import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Misc exposing (..)
import Task exposing (Task)
import Ucb.Main.Message exposing (Message(..))
import Ucb.Main.Model exposing (..)
import Ucb.Main.View exposing (view)
import Ucb.Unison.BranchDict exposing (..)
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


main : Program () Model Message
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Message )
init _ =
    let
        model : Model
        model =
            { api =
                { unison = makeLocalServerUnisonCodebaseAPI
                }
            , codebase =
                { head = Nothing
                , branches = emptyBranchDict
                , terms = HashDict.empty referentEquality referentHashing
                , termTypes = HashDict.empty referentEquality referentHashing
                , types = HashDict.empty idEquality idHashing
                , parents = emptyBranchDict
                , successors = emptyBranchDict
                }
            , ui =
                { branches = emptyBranchDict
                , terms = HashDict.empty referentEquality referentHashing
                }
            , errors = []
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
            updateHttpGetHeadHash result model

        Http_GetBranch result ->
            updateHttpGetBranch result model

        Http_GetTerm result ->
            updateHttpGetTerm result model

        Http_GetTermType result ->
            updateHttpGetTermType result model

        Http_GetType result ->
            updateHttpGetType result model

        Http_GetTypes result ->
            updateHttpGetTypes result model

        User_FocusBranch hash ->
            updateUserFocusBranch hash model

        User_GetType reference ->
            updateUserGetType reference model

        User_GetTerm referent ->
            updateUserGetTerm referent model

        User_ToggleBranch hash ->
            updateUserToggleBranch hash model

        User_DebugButton ->
            updateUserDebugButton model


{-| Whatever you're debugging. Might be nothing!
-}
updateUserDebugButton :
    Model
    -> ( Model, Cmd Message )
updateUserDebugButton model =
    ( model, Cmd.none )


{-| Got the head hash. Next step: get the actual (decoded) bytes.
-}
updateHttpGetHeadHash :
    Result (Http.Error String) (Http.Response BranchHash)
    -> Model
    -> ( Model, Cmd Message )
updateHttpGetHeadHash result model =
    case result of
        Err err ->
            ( { model | errors = Err_GetHeadHash err :: model.errors }
            , Cmd.none
            )

        Ok response ->
            updateHttpGetHeadHash2 response model


updateHttpGetHeadHash2 :
    Http.Response BranchHash
    -> Model
    -> ( Model, Cmd Message )
updateHttpGetHeadHash2 response model =
    ( model
    , getBranch
        model.api.unison
        model.codebase
        response.body
        |> Task.attempt Http_GetBranch
    )


updateHttpGetBranch :
    Result (Http.Error Bytes)
        ( BranchHash
        , { branches : BranchDict Branch
          , parents : BranchDict (HashSet BranchHash)
          , successors : BranchDict (HashSet BranchHash)
          }
        )
    -> Model
    -> ( Model, Cmd Message )
updateHttpGetBranch result model =
    case result of
        Err err ->
            ( { model | errors = Err_GetBranch err :: model.errors }
            , Cmd.none
            )

        Ok result2 ->
            updateHttpGetBranch2 result2 model


updateHttpGetBranch2 :
    ( BranchHash
    , { branches : HashDict BranchHash Branch
      , parents : HashDict BranchHash (HashSet BranchHash)
      , successors : HashDict BranchHash (HashSet BranchHash)
      }
    )
    -> Model
    -> ( Model, Cmd Message )
updateHttpGetBranch2 ( hash, { branches, parents, successors } ) model =
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
                (branchDictMonoid hashSetSemigroup).semigroup.prepend
                    parents
                    model.codebase.parents
            , successors =
                (branchDictMonoid hashSetSemigroup).semigroup.prepend
                    successors
                    model.codebase.successors
            , terms = model.codebase.terms
            , termTypes = model.codebase.termTypes
            , types = model.codebase.types
            }
      }
    , case HashDict.get hash newBranches of
        -- This should never be the case
        Nothing ->
            Cmd.none

        Just branch ->
            getMissingTypes model branch
                |> Task.attempt Http_GetTypes
    )


updateHttpGetTerm :
    Result (Http.Error Bytes) ( Id, Http.Response (Term Symbol) )
    -> Model
    -> ( Model, Cmd Message )
updateHttpGetTerm result model =
    case result of
        Err err ->
            ( { model | errors = Err_GetTerm err :: model.errors }
            , Cmd.none
            )

        Ok response ->
            updateHttpGetTerm2 response model


updateHttpGetTerm2 :
    ( Id, Http.Response (Term Symbol) )
    -> Model
    -> ( Model, Cmd Message )
updateHttpGetTerm2 ( id, response ) model =
    let
        command : Cmd Message
        command =
            case HashDict.get (Ref (Derived id)) model.codebase.termTypes of
                Nothing ->
                    model.api.unison.getTermType id
                        |> Task.attempt Http_GetTermType

                Just _ ->
                    Cmd.none
    in
    ( { model
        | codebase =
            { terms =
                HashDict.insert
                    (Ref (Derived id))
                    response.body
                    model.codebase.terms

            -- unchanged
            , head = model.codebase.head
            , branches = model.codebase.branches
            , termTypes = model.codebase.termTypes
            , types = model.codebase.types
            , parents = model.codebase.parents
            , successors = model.codebase.successors
            }
      }
    , command
    )


updateHttpGetTermType :
    Result (Http.Error Bytes) ( Id, Http.Response (Type Symbol) )
    -> Model
    -> ( Model, Cmd message )
updateHttpGetTermType result model =
    case result of
        Err err ->
            ( { model | errors = Err_GetTermType err :: model.errors }
            , Cmd.none
            )

        Ok response ->
            updateHttpGetTermType2 response model


updateHttpGetTermType2 :
    ( Id, Http.Response (Type Symbol) )
    -> Model
    -> ( Model, Cmd message )
updateHttpGetTermType2 ( id, response ) model =
    ( { model
        | codebase =
            { termTypes =
                HashDict.insert
                    (Ref (Derived id))
                    response.body
                    model.codebase.termTypes

            -- unchanged
            , head = model.codebase.head
            , branches = model.codebase.branches
            , terms = model.codebase.terms
            , types = model.codebase.types
            , parents = model.codebase.parents
            , successors = model.codebase.successors
            }
      }
    , Cmd.none
    )


updateHttpGetType :
    Result (Http.Error Bytes) ( Id, Http.Response (Declaration Symbol) )
    -> Model
    -> ( Model, Cmd message )
updateHttpGetType result model =
    case result of
        Err err ->
            ( { model | errors = Err_GetType err :: model.errors }
            , Cmd.none
            )

        Ok response ->
            updateHttpGetType2 response model


updateHttpGetType2 :
    ( Id, Http.Response (Declaration Symbol) )
    -> Model
    -> ( Model, Cmd message )
updateHttpGetType2 ( id, response ) model =
    ( { model
        | codebase =
            { types =
                HashDict.insert
                    id
                    response.body
                    model.codebase.types

            -- unchanged
            , head = model.codebase.head
            , branches = model.codebase.branches
            , terms = model.codebase.terms
            , termTypes = model.codebase.termTypes
            , parents = model.codebase.parents
            , successors = model.codebase.successors
            }
      }
    , Cmd.none
    )


updateHttpGetTypes :
    Result (Http.Error Bytes) (List ( Id, Declaration Symbol ))
    -> Model
    -> ( Model, Cmd message )
updateHttpGetTypes result model =
    case result of
        Err err ->
            ( { model | errors = Err_GetTypes err :: model.errors }
            , Cmd.none
            )

        Ok response ->
            updateHttpGetTypes2 response model


updateHttpGetTypes2 :
    List ( Id, Declaration Symbol )
    -> Model
    -> ( Model, Cmd message )
updateHttpGetTypes2 types model =
    ( { model
        | codebase =
            { types =
                List.foldl
                    (\( id, declaration ) ->
                        HashDict.insert
                            id
                            declaration
                    )
                    model.codebase.types
                    types

            -- unchanged
            , head = model.codebase.head
            , branches = model.codebase.branches
            , terms = model.codebase.terms
            , termTypes = model.codebase.termTypes
            , parents = model.codebase.parents
            , successors = model.codebase.successors
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
updateUserFocusBranch :
    BranchHash
    -> Model
    -> ( Model, Cmd Message )
updateUserFocusBranch hash model =
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
                    , types = model.codebase.types
                    , parents = model.codebase.parents
                    , successors = model.codebase.successors
                    }
              }
            , getMissingTypes model branch
                |> Task.attempt Http_GetTypes
            )


{-| Fetch the term if we haven't already
-}
updateUserGetTerm :
    Referent
    -> Model
    -> ( Model, Cmd Message )
updateUserGetTerm referent model =
    case referent of
        Ref reference ->
            case reference of
                Builtin _ ->
                    ( model, Cmd.none )

                Derived id ->
                    let
                        command : Cmd Message
                        command =
                            case HashDict.get referent model.codebase.terms of
                                Nothing ->
                                    model.api.unison.getTerm id
                                        |> Task.attempt Http_GetTerm

                                Just _ ->
                                    Cmd.none

                        newTerms : HashDict Referent Bool
                        newTerms =
                            HashDict.update
                                referent
                                (maybe True not >> Just)
                                model.ui.terms
                    in
                    ( { model
                        | ui =
                            { terms = newTerms

                            -- unchanged
                            , branches = model.ui.branches
                            }
                      }
                    , command
                    )

        Con _ _ _ ->
            ( model, Cmd.none )


{-| Fetch the type if we haven't already
-}
updateUserGetType :
    Id
    -> Model
    -> ( Model, Cmd Message )
updateUserGetType id model =
    let
        command : Cmd Message
        command =
            case HashDict.get id model.codebase.types of
                Nothing ->
                    model.api.unison.getType id
                        |> Task.attempt Http_GetType

                Just _ ->
                    Cmd.none
    in
    ( model
    , command
    )


updateUserToggleBranch :
    BranchHash
    -> Model
    -> ( Model, Cmd Message )
updateUserToggleBranch hash model =
    ( { model
        | ui =
            { branches =
                HashDict.update
                    hash
                    (maybe True not >> Just)
                    model.ui.branches

            -- unchanged
            , terms = model.ui.terms
            }
      }
    , case HashDict.get hash model.codebase.branches of
        -- Should never be the case
        Nothing ->
            Cmd.none

        Just branch ->
            getMissingTypes model branch
                |> Task.attempt Http_GetTypes
    )


subscriptions :
    Model
    -> Sub Message
subscriptions _ =
    Sub.none
