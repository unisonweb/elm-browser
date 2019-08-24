module Ucb.Main.View.Referent exposing (viewReferent)

import Element exposing (..)
import Element.Font exposing (..)
import Misc exposing (maybe)
import Ucb.Main.View.Reference exposing (viewReference)
import Unison.Referent exposing (..)


viewReferent :
    { showBuiltin : Bool
    , take : Maybe Int
    }
    -> Referent
    -> Element message
viewReferent opts referent =
    case referent of
        Ref reference ->
            viewReference opts reference

        Con reference _ _ ->
            viewReference opts reference
