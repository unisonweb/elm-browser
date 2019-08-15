-- | Parse an array as a tuple.


module Ucb.Util.ArrayParser exposing
    ( ArrayParser
    , parse
    , pure
    , run
    )

import Array exposing (Array)


type ArrayParser z a
    = ArrayParser (Array z -> Maybe ( a, Array z ))


run :
    ArrayParser z a
    -> Array z
    -> Maybe a
run (ArrayParser p) zs =
    Maybe.map Tuple.first (p zs)


pure : a -> ArrayParser z a
pure x =
    ArrayParser (\zs -> Just ( x, zs ))


{-| Parse a @z@ into an @a@ at the current index, and advance it by 1.
-}
parse :
    (z -> Maybe a)
    -> ArrayParser z (a -> b)
    -> ArrayParser z b
parse f (ArrayParser p) =
    ArrayParser
        (\zs ->
            case p zs of
                Nothing ->
                    Nothing

                Just ( g, zs2 ) ->
                    case Array.get 0 zs2 of
                        Nothing ->
                            Nothing

                        Just z ->
                            Maybe.map
                                (\a -> ( g a, Array.slice 1 (Array.length zs2) zs2 ))
                                (f z)
        )
