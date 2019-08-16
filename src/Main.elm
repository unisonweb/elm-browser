module Main exposing (..)

import Browser
import Http
import Task
import Ucb.Main.Message exposing (Message(..))
import Ucb.Main.Model exposing (Model)
import Ucb.Main.View exposing (view)
import Ucb.Unison.Codebase.Path exposing (httpGetHeadPath)


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
    ( { result = Nothing }
    , httpGetHeadPath "unisonweb" "unisonbase"
        |> Task.attempt DownloadedHeadPath
    )


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        DownloadedHeadPath result ->
            ( { model | result = Just result }, Cmd.none )
