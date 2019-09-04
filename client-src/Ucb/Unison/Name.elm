module Ucb.Unison.Name exposing (..)

import HashingContainers.HashDict as HashDict
import HashingContainers.HashSet as HashSet exposing (HashSet)
import Ucb.Unison.NameDict exposing (NameDict)
import Unison.Name exposing (..)


shortenName :
    NameDict (HashSet a)
    -> Name
    -> Name
shortenName nameToThings fullName =
    let
        -- Shorten "foo.bar.Baz" as much as possible by picking the shortest
        -- unambiguous name. Note that the full name might be ambiguous, that's
        -- okay.
        shorten : List Name -> Name
        shorten candidates =
            case candidates of
                [] ->
                    fullName

                name :: names ->
                    case HashDict.get name nameToThings of
                        Nothing ->
                            fullName

                        Just things ->
                            if HashSet.size things == 1 then
                                name

                            else
                                shorten names
    in
    shorten (List.reverse (nameTails fullName))
