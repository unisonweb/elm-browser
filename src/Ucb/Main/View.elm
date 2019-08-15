module Ucb.Main.View exposing (..)

import Element exposing (..)
import Html exposing (Html)
import Ucb.Main.Message exposing (Message)
import Ucb.Main.Model exposing (Model)
import Ucb.Unison.Codebase exposing (Codebase)


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
                [ text "I'm downloading unisonweb/unisonbase" ]

            Just (Err err) ->
                [ text "I tried downloading unisonweb/unisonbase but it failed:"
                , text (Debug.toString err)
                ]

            Just (Ok codebase) ->
                [ text "I downloaded unisonweb/unisonbase and it looks like this on one line:"
                , text (Debug.toString codebase)
                , text "But cleaned up a bit it's basically:"
                , viewCodebase codebase
                ]
        )


viewCodebase : Codebase -> Element Message
viewCodebase codebase =
    column
        []
        [ row [ spacing 10 ] [ text "dependents", text codebase.dependents.git_url ]
        , row [ spacing 10 ] [ text "patches", text codebase.patches.git_url ]
        , row [ spacing 10 ] [ text "terms", text codebase.terms.git_url ]
        , row [ spacing 10 ] [ text "type-index", text codebase.type_index.git_url ]
        , row [ spacing 10 ] [ text "type-mentions-index", text codebase.type_mentions_index.git_url ]
        , row [ spacing 10 ] [ text "types", text codebase.types.git_url ]
        ]
