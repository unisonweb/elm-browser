module Main exposing (..)

-- Unison.* imports are unused, but there temporarily so 'elm make' typechecks

import Browser
import Dict
import HashingContainers.HashDict as HashDict
import HashingContainers.HashSet as HashSet
import Task
import Ucb.Main.Message exposing (Message(..))
import Ucb.Main.Model exposing (..)
import Ucb.Main.View exposing (view)
import Ucb.Unison.Codebase.API exposing (..)
import Ucb.Unison.Codebase.API.GitHub exposing (..)
import Ucb.Util.Http as Http
import Unison.Codebase.Causal exposing (..)
import Unison.Hash exposing (..)
import Unison.Reference exposing (..)
import Unison.Symbol exposing (..)
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
                { unison = makeGitHubUnisonCodebaseAPI "unisonweb" "unisonbase"
                }
            , codebase =
                { head = Nothing
                , branches = HashDict.empty hash32Equality hash32Hashing
                , types = HashDict.empty idEquality idHashing
                , parents = HashDict.empty hash32Equality hash32Hashing
                , successors = HashDict.empty hash32Equality hash32Hashing
                }
            , ui =
                { branches = HashDict.empty hash32Equality hash32Hashing
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

        Http_GetType result ->
            updateHttpGetType result model

        User_GetBranch payload ->
            updateUserGetBranch payload model

        User_GetType reference ->
            updateUserGetType reference model


{-| Got the head hash. Next step: get the actual (decoded) bytes.
-}
updateHttpGetHeadHash :
    Result GetHeadHashError (Http.Response Hash32)
    -> Model
    -> ( Model, Cmd Message )
updateHttpGetHeadHash result model =
    case result of
        Err err ->
            ( accumulateError (Err_GetHeadHash err) model
            , Cmd.none
            )

        Ok response ->
            ( { model
                | codebase =
                    { head = Just response.body
                    , branches = model.codebase.branches
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
    Result GetRawCausalError ( Hash32, Http.Response RawCausal )
    -> Model
    -> ( Model, Cmd message )
updateHttpGetRawCausal result model =
    case result of
        Err err ->
            ( accumulateError (Err_GetRawCausal err) model
            , Cmd.none
            )

        Ok ( hash, response ) ->
            ( { model
                | codebase =
                    { -- Head doesn't change
                      head = model.codebase.head

                    -- Store branch
                    , branches =
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

                    -- Types don't change
                    , types = model.codebase.types
                    }
              }
            , Cmd.none
            )


updateHttpGetType :
    Result GetTypeError ( Id, Http.Response (Type Symbol) )
    -> Model
    -> ( Model, Cmd message )
updateHttpGetType result model =
    case result of
        Err err ->
            ( accumulateError (Err_GetType err) model
            , Cmd.none
            )

        Ok ( id, response ) ->
            ( model, Cmd.none )


{-| Fetch the branch if we haven't already, and possibly focus it.
-}
updateUserGetBranch :
    { hash : Hash32, focus : Bool }
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
                , branches = model.codebase.branches
                , types = model.codebase.types
                , parents = model.codebase.parents
                , successors = model.codebase.successors
                }
          }
        , command
        )

    else
        ( { model
            | ui =
                case HashDict.get hash model.ui.branches of
                    Nothing ->
                        { branches =
                            HashDict.insert hash True model.ui.branches
                        }

                    Just show ->
                        { branches =
                            HashDict.insert hash (not show) model.ui.branches
                        }
          }
        , command
        )


{-| Fetch the type if we haven't already
-}
updateUserGetType :
    Reference
    -> Model
    -> ( Model, Cmd Message )
updateUserGetType reference model =
    case reference of
        -- Nothing to do for builtins? If so, TODO don't even send this message.
        Builtin _ ->
            ( model, Cmd.none )

        Derived id ->
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
            ( model, command )
