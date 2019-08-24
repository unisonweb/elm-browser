module Main exposing (..)

import Browser
import Bytes exposing (Bytes)
import Dict
import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet
import Task exposing (Task)
import Ucb.Main.Message exposing (Message(..))
import Ucb.Main.Model exposing (..)
import Ucb.Main.View exposing (view)
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
                , branches = HashDict.empty hash32Equality hash32Hashing
                , branches2 = HashDict.empty hash32Equality hash32Hashing
                , terms = HashDict.empty referentEquality referentHashing
                , termTypes = HashDict.empty referentEquality referentHashing
                , types = HashDict.empty referenceEquality referenceHashing
                , parents = HashDict.empty hash32Equality hash32Hashing
                , successors = HashDict.empty hash32Equality hash32Hashing
                }
            , ui =
                { branches = HashDict.empty hash32Equality hash32Hashing
                , terms = HashDict.empty referentEquality referentHashing
                , types = HashDict.empty referenceEquality referenceHashing
                }
            , errors = []
            , rateLimit = Nothing
            }

        -- First command: fetch _head path!
        initialCommand : Cmd Message
        initialCommand =
            model.api.unison.getHeadHash
                |> Task.attempt Http_GetHeadHash
    in
    ( model, initialCommand )


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Http_GetHeadHash result ->
            updateHttpGetHeadHash result model

        Http_GetRawCausal result ->
            updateHttpGetRawCausal result model

        Http_GetTerm result ->
            updateHttpGetTerm result model

        Http_GetTermType result ->
            updateHttpGetTermType result model

        Http_GetType result ->
            updateHttpGetType result model

        User_GetBranch payload ->
            updateUserGetBranch payload model

        User_GetType reference ->
            updateUserGetType reference model

        User_GetTerm referent ->
            updateUserGetTerm referent model


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
    ( { model
        | codebase =
            { head = Just response.body

            -- unchanged
            , branches = model.codebase.branches
            , branches2 = model.codebase.branches2
            , terms = model.codebase.terms
            , termTypes = model.codebase.termTypes
            , types = model.codebase.types
            , parents = model.codebase.parents
            , successors = model.codebase.successors
            }
        , rateLimit =
            Dict.get "x-ratelimit-remaining" response.headers
      }
    , model.api.unison.getRawCausal response.body
        |> Task.attempt Http_GetRawCausal
    )


{-| We received a RawCausal from the sky. This happens once initially (\_head
branch), and then every time the user requests one to be fetched via the UI.
What do we do with all these branches? Just store them forever in a map.
-}
updateHttpGetRawCausal :
    Result (Http.Error Bytes) ( BranchHash, Http.Response (RawCausal RawBranch) )
    -> Model
    -> ( Model, Cmd message )
updateHttpGetRawCausal result model =
    case result of
        Err err ->
            ( { model | errors = Err_GetRawCausal err :: model.errors }
            , Cmd.none
            )

        Ok response ->
            updateHttpGetRawCausal2 response model


updateHttpGetRawCausal2 :
    ( BranchHash, Http.Response (RawCausal RawBranch) )
    -> Model
    -> ( Model, Cmd message )
updateHttpGetRawCausal2 ( hash, response ) model =
    ( { model
        | codebase =
            { -- Store branch
              branches =
                HashDict.insert
                    hash
                    response.body
                    model.codebase.branches

            -- Add the child->this mappings
            , parents =
                insertParents
                    hash
                    (response.body
                        |> rawCausalHead
                        |> .children
                        |> HashDict.toList
                        |> List.map Tuple.second
                    )
                    model.codebase.parents

            -- And the predecessor->this mappings
            , successors =
                insertSuccessors
                    hash
                    (rawCausalPredecessors response.body)
                    model.codebase.successors

            -- unchanged
            , head = model.codebase.head
            , branches2 = model.codebase.branches2
            , terms = model.codebase.terms
            , termTypes = model.codebase.termTypes
            , types = model.codebase.types
            }
      }
    , Cmd.none
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
            , branches2 = model.codebase.branches2
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
            , branches2 = model.codebase.branches2
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
                    (Derived id)
                    response.body
                    model.codebase.types

            -- unchanged
            , head = model.codebase.head
            , branches = model.codebase.branches
            , branches2 = model.codebase.branches2
            , terms = model.codebase.terms
            , termTypes = model.codebase.termTypes
            , parents = model.codebase.parents
            , successors = model.codebase.successors
            }
      }
    , Cmd.none
    )


{-| Fetch the branch if we haven't already, and possibly focus it.
-}
updateUserGetBranch :
    { hash : BranchHash, focus : Bool }
    -> Model
    -> ( Model, Cmd Message )
updateUserGetBranch { hash, focus } model =
    let
        command : Cmd Message
        command =
            case HashDict.get hash model.codebase.branches of
                Nothing ->
                    model.api.unison.getRawCausal hash
                        |> Task.attempt Http_GetRawCausal

                Just _ ->
                    Cmd.none
    in
    if focus then
        ( { model
            | codebase =
                { head = Just hash

                -- unchanged
                , branches = model.codebase.branches
                , branches2 = model.codebase.branches2
                , terms = model.codebase.terms
                , termTypes = model.codebase.termTypes
                , types = model.codebase.types
                , parents = model.codebase.parents
                , successors = model.codebase.successors
                }
          }
        , command
        )

    else
        let
            newBranches : HashDict BranchHash Bool
            newBranches =
                HashDict.update
                    hash
                    (\maybeVisible ->
                        case maybeVisible of
                            Nothing ->
                                Just True

                            Just visible ->
                                Just (not visible)
                    )
                    model.ui.branches
        in
        ( { model
            | ui =
                { branches = newBranches

                -- unchanged
                , terms = model.ui.terms
                , types = model.ui.types
                }
          }
        , command
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
                                (\maybeVisible ->
                                    case maybeVisible of
                                        Nothing ->
                                            Just True

                                        Just visible ->
                                            Just (not visible)
                                )
                                model.ui.terms
                    in
                    ( { model
                        | ui =
                            { terms = newTerms

                            -- unchanged
                            , branches = model.ui.branches
                            , types = model.ui.types
                            }
                      }
                    , command
                    )

        Con _ _ _ ->
            ( model, Cmd.none )


{-| Fetch the type if we haven't already
-}
updateUserGetType :
    Reference
    -> Model
    -> ( Model, Cmd Message )
updateUserGetType reference model =
    case reference of
        Builtin _ ->
            ( model, Cmd.none )

        Derived id ->
            let
                command : Cmd Message
                command =
                    case HashDict.get reference model.codebase.types of
                        Nothing ->
                            model.api.unison.getType id
                                |> Task.attempt Http_GetType

                        Just _ ->
                            Cmd.none

                newTypes : HashDict Reference Bool
                newTypes =
                    HashDict.update
                        reference
                        (\maybeVisible ->
                            case maybeVisible of
                                Nothing ->
                                    Just True

                                Just visible ->
                                    Just (not visible)
                        )
                        model.ui.types
            in
            ( { model
                | ui =
                    { types = newTypes

                    -- unchanged
                    , branches = model.ui.branches
                    , terms = model.ui.terms
                    }
              }
            , command
            )
