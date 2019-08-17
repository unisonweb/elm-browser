-- | Replace with nikita-volkov/typeclasses when I can install it.
-- See https://github.com/nikita-volkov/typeclasses/issues/1


module HashingContainers exposing (..)


type HashDict k v
    = Undefined1


type HashSet a
    = Undefined2


type alias Hashing a =
    { hash : a -> Int
    , hashWithSalt : Int -> a -> Int
    }


hash : (a -> Int) -> Hashing a
hash f =
    { hash = f
    , hashWithSalt = always f -- temp impl, so who cares. ignore salt.
    }


type alias Equality a =
    { eq : a -> a -> Bool
    , notEq : a -> a -> Bool
    }


eq : (a -> a -> Bool) -> Equality a
eq f =
    { eq = f
    , notEq = \x y -> not (f x y)
    }
