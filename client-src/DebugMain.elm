module Main exposing (..)

import Browser
import Bytes exposing (Bytes)
import Dict
import HashingContainers.HashDict as HashDict exposing (HashDict)
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Html exposing (Html)
import Misc exposing (..)
import Task exposing (Task)
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


type alias Model =
    Maybe (Result (Http.Error Bytes) ( Id, Http.Response (Type Symbol) ))


type Message
    = Message (Result (Http.Error Bytes) ( Id, Http.Response (Type Symbol) ))


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
        initialCommand : Cmd Message
        initialCommand =
            makeLocalServerUnisonCodebaseAPI.getTermType
                { hash = "hoa6tis7604fo17o91lfjflsvd843uvm7sueu5rfqqn5nbneajoh9fldfbmmjnhh1h2690ktfflrbb96q9lksesuh3t5amcc1ph92g"
                , pos = 0
                , size = 1
                }
                |> Task.attempt Message
    in
    ( Nothing, initialCommand )


update : Message -> Model -> ( Model, Cmd Message )
update (Message message) model =
    ( Just message, Cmd.none )


view : Model -> Html Message
view model =
    Html.text (Debug.toString model)


subscriptions :
    Model
    -> Sub Message
subscriptions _ =
    Sub.none
