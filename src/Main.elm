module Main exposing (..)

import Browser
import Http
import Ucb.Main.Message exposing (Message(..))
import Ucb.Main.Model exposing (Model)
import Ucb.Main.View exposing (view)
import Ucb.Unison.Codebase exposing (Codebase, GetCodebaseError, getCodebase)


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
    , Cmd.map DownloadedCodebase (getCodebase "unisonweb" "unisonbase")
    )


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        DownloadedCodebase result ->
            ( { model | result = Just result }, Cmd.none )
