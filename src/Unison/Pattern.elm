module Unison.Pattern exposing (..)


type Pattern ann
    = Undefined1 ann


type SeqOp
    = Cons
    | Snoc
    | Concat
