module Ucb.Main.View.Reference exposing (viewReference)

import Element exposing (..)
import Element.Font exposing (..)
import Misc exposing (maybe)
import Unison.Reference exposing (..)


viewReference :
    { showBuiltin : Bool
    , take : Maybe Int
    }
    -> Reference
    -> Element message
viewReference { showBuiltin, take } reference =
    case reference of
        Builtin name ->
            if showBuiltin then
                text name

            else
                none

        Derived id ->
            viewId take id


viewId : Maybe Int -> Id -> Element message
viewId take { hash, pos, size } =
    row
        []
        [ el [ color (rgb 0.5 0.5 0.5) ]
            (text (maybe identity String.left take hash))
        , if size > 1 then
            el
                [ color (rgb 0.5 0.5 0.5) ]
                (text (String.cons '#' (String.fromInt pos)))

          else
            none
        ]
