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
import Ucb.Unison.Codebase.Path exposing (..)
import Unison.Codebase.Causal exposing (..)
import Unison.Hash exposing (..)


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
        initialModel : Model
        initialModel =
            { head = Nothing
            , codebase =
                { branches = HashDict.empty hash32Equality hash32Hashing
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
            httpGetHeadHash owner repo
                |> Task.attempt Http_GetHeadHash
    in
    ( initialModel, initialCommand )


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        -- Got the head hash. Next step: get the actual (decoded) bytes.
        Http_GetHeadHash result ->
            case result of
                Err err ->
                    ( accumulateError (Err_GetHeadHash err) model
                    , Cmd.none
                    )

                Ok response ->
                    ( { model
                        | head = Just response.body
                        , rateLimit =
                            Dict.get "x-ratelimit-remaining" response.headers
                      }
                    , httpGetRawCausal owner repo response.body
                        |> Task.attempt Http_GetRawCausal
                    )

        -- We received a RawCausal from the sky. This happens once initially
        -- (_head branch), and then every time the user requests one to be
        -- fetched via the UI.
        --
        -- What do we do with all these branches? Just store them forever in
        -- a map.
        Http_GetRawCausal result ->
            case result of
                Err err ->
                    ( accumulateError (Err_GetRawCausal err) model
                    , Cmd.none
                    )

                Ok ( hash, response ) ->
                    ( { model
                        | codebase =
                            -- Store branch
                            { branches =
                                HashDict.insert
                                    hash
                                    response.body
                                    model.codebase.branches

                            -- And the predecessor->this-branch mappings
                            , successors =
                                insertSuccessors
                                    hash
                                    (rawCausalPredecessors response.body)
                                    model.codebase.successors
                            }
                      }
                    , Cmd.none
                    )

        -- Fetch the branch if we haven't already, and possibly focus it.
        User_GetBranch { hash, focus } ->
            let
                command : Cmd Message
                command =
                    case HashDict.get hash model.codebase.branches of
                        Nothing ->
                            httpGetRawCausal owner repo hash
                                |> Task.attempt Http_GetRawCausal

                        Just _ ->
                            Cmd.none
            in
            if focus then
                ( { model | head = Just hash }, command )

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


{-| Hard-coded test repo, not permanent.
-}
owner : String
owner =
    "unisonweb"


{-| Hard-coded test repo, not permanent.
-}
repo : String
repo =
    "unisonbase"
