module Scratch exposing (..)


ins : Maybe String -> Maybe String
ins xs =
    case xs of
        Nothing ->
            Just "foo"

        Just _ ->
            Just "bar"
