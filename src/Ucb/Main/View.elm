module Ucb.Main.View exposing (..)

import Bytes
import Element exposing (..)
import Html exposing (Html)
import Ucb.Main.Message exposing (Message)
import Ucb.Main.Model exposing (Error(..), Model)
import Unison.Codebase.Causal exposing (..)


view : Model -> Html Message
view model =
    layout
        [ padding 10 ]
        (view2 model)


view2 : Model -> Element Message
view2 model =
    column
        [ spacing 10 ]
        (List.filterMap identity
            [ Maybe.map
                (\hash ->
                    column []
                        [ text "Head hash:"
                        , text hash
                        ]
                )
                model.headHash
            , Maybe.map
                (\causal ->
                    column []
                        [ text "Head:"
                        , viewRawCausal causal
                        ]
                )
                model.head
            , if List.isEmpty model.errors then
                Nothing

              else
                Just
                    (column []
                        (text "Errors:"
                            :: List.map viewError (List.reverse model.errors)
                        )
                    )
            ]
        )


viewError :
    Error
    -> Element message
viewError error =
    text (Debug.toString error)


viewRawCausal :
    RawCausal
    -> Element message
viewRawCausal causal =
    text (Debug.toString causal)


viewMaybe :
    (a -> Element message)
    -> Maybe a
    -> Element message
viewMaybe f mx =
    case mx of
        Nothing ->
            none

        Just x ->
            f x
