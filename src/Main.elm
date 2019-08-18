module Main exposing (..)

-- Unison.* imports are unused, but there temporarily so 'elm make' typechecks

import Browser
import HashingContainers.HashDict as HashDict
import Task
import Ucb.Main.Message exposing (Message(..))
import Ucb.Main.Model exposing (..)
import Ucb.Main.View exposing (view)
import Ucb.Unison.Codebase.Path exposing (..)
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
                { branches =
                    HashDict.empty hash32Equality hash32Hashing
                }
            , errors = []
            }

        initialCommand : Cmd Message
        initialCommand =
            httpGetHeadHash owner repo
                |> Task.attempt GetHeadHash
    in
    ( initialModel, initialCommand )


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        -- Fetch the branch if we haven't already.
        ClickBranchHash hash ->
            case HashDict.get hash model.codebase.branches of
                Nothing ->
                    ( model
                    , httpGetRawCausal owner repo hash
                        |> Task.attempt GetRawCausal
                    )

                Just _ ->
                    ( model, Cmd.none )

        GetHeadHash result ->
            case result of
                Err err ->
                    ( accumulateError (Err_GetHeadHash err) model
                    , Cmd.none
                    )

                Ok response ->
                    ( { model
                        | head = Just response.body
                      }
                    , httpGetRawCausal owner repo response.body
                        |> Task.attempt GetRawCausal
                    )

        GetRawCausal result ->
            case result of
                Err err ->
                    ( accumulateError (Err_GetRawCausal err) model
                    , Cmd.none
                    )

                Ok ( hash, response ) ->
                    ( { model
                        | codebase =
                            { branches =
                                HashDict.insert
                                    hash
                                    response.body
                                    model.codebase.branches
                            }
                      }
                    , Cmd.none
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
