module Ucb.Main.View exposing (..)

import Element exposing (..)
import Html exposing (Html)
import Ucb.Main.Message exposing (Message)
import Ucb.Main.Model exposing (Model)


view : Model -> Html Message
view model =
    layout
        [ padding 10 ]
        (view2 model)


view2 : Model -> Element Message
view2 model =
    column
        [ spacing 10 ]
        (case model.result of
            Nothing ->
                [ text "I'm downloading unisonweb/unisonbase/.unison/v1/paths/_head" ]

            Just (Err err) ->
                [ text "I tried downloading unisonweb/unisonbase/.unison/v1/paths/_head but it failed:"
                , text (Debug.toString err)
                ]

            Just (Ok path) ->
                [ text "I downloaded unisonweb/unisonbase/.unison/v1/paths/_head"
                , text path
                ]
        )
