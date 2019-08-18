module Ucb.Main.View exposing (..)

import Bytes
import Element exposing (..)
import Html exposing (Html)
import Ucb.Main.Message exposing (Message)
import Ucb.Main.Model exposing (Model)
import Unison.Codebase.Serialization.V1 exposing (decodeRawCausal)


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
                [ text "I'm downloading the head path of unisonweb/unisonbase" ]

            Just (Err err) ->
                [ text "I tried downloading the head path of unisonweb/unisonbase it failed:"
                , text (Debug.toString err)
                ]

            Just (Ok bytes) ->
                [ text "I downloaded the head path of unisonweb/unisonbase"
                , text ("It's " ++ String.fromInt (Bytes.width bytes) ++ " bytes")
                , text "Now I'm going to try to parse it."
                , text (Debug.toString (decodeRawCausal bytes))
                ]
        )
