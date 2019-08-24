module Ucb.Main.View exposing (..)

import Element exposing (..)
import HashingContainers.HashDict as HashDict exposing (HashDict)
import Html exposing (Html)
import Ucb.Main.Message exposing (..)
import Ucb.Main.Model exposing (..)
import Ucb.Main.View.Branch exposing (..)


view : Model -> Html Message
view model =
    layout [] (view2 model)


view2 : Model -> Element Message
view2 model =
    column
        []
        (List.filterMap identity
            [ model.codebase.head
                |> Maybe.andThen
                    (\head ->
                        Maybe.map
                            (viewBranch model head)
                            (HashDict.get head model.codebase.branches)
                    )
            , if List.isEmpty model.errors then
                Nothing

              else
                Just
                    (column []
                        (text "Errors:"
                            :: List.map viewError (List.reverse model.errors)
                        )
                    )
            , Maybe.map
                (\limit -> text ("GitHub rate limit: " ++ limit))
                model.rateLimit
            ]
        )


viewError :
    Error
    -> Element message
viewError error =
    text (Debug.toString error)
