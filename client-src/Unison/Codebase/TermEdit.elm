module Unison.Codebase.TermEdit exposing (..)

import Misc exposing (tumble)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import Unison.Reference exposing (..)


{-| Haskell type: Unison.Codebase.TermEdit.TermEdit
-}
type TermEdit
    = TermEditReplace Reference Typing
    | TermEditDeprecate


termEditEquality : Equality TermEdit
termEditEquality =
    Equality.eq (==)


termEditHashing : Hashing TermEdit
termEditHashing =
    Hashing.hash
        (\edit ->
            case edit of
                TermEditReplace reference typing ->
                    referenceHashing.hash reference
                        |> tumble (typingHashing.hash typing)

                TermEditDeprecate ->
                    1
        )


{-| Haskell type: Unison.Codebase.TermEdit.Typing
-}
type Typing
    = Same
    | Subtype
    | Different


typingHashing : Hashing Typing
typingHashing =
    Hashing.hash
        (\typing ->
            case typing of
                Same ->
                    0

                Subtype ->
                    1

                Different ->
                    2
        )
