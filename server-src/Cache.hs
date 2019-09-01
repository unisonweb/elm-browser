module Cache
  ( Cache
  , Key(..)
  , newCache
  , readCache
  , writeCache
  ) where

import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.IORef
import Data.Text (Text)
import Numeric.Natural (Natural)
import GHC.Generics (Generic)

import qualified Data.HashPSQ as PSQ


type PSQ
  = PSQ.HashPSQ

data Key
  = KeyBranch Text
  | KeyDeclaration Text
  | KeyTerm Text
  | KeyTermType Text
  deriving stock (Eq, Generic, Ord)
  deriving anyclass (Hashable)

type Cache
  = IORef (Natural, Natural, PSQ Key Natural ByteString)

-- | Hard-coded cache capacity. Oldest entries are evicted.
capacity :: Natural
capacity =
  4096

newCache :: IO Cache
newCache =
  newIORef (0, 0, PSQ.empty)

readCache :: Cache -> Key -> IO (Maybe ByteString)
readCache ref key =
  atomicModifyIORef' ref (readCache_ key)

readCache_
  :: Key
  -> (Natural, Natural, PSQ Key Natural ByteString)
  -> ((Natural, Natural, PSQ Key Natural ByteString), Maybe ByteString)
readCache_ key (now, size, cache) =
  let
    result :: Maybe ByteString
    cache2 :: PSQ Key Natural ByteString
    (result, cache2) =
      PSQ.alter
        (\case
          Nothing -> (Nothing, Nothing)
          Just (_, value) -> (Just value, Just (now, value)))
        key
        cache

    now2 :: Natural
    now2 =
      now + 1
  in
    now2 `seq` ((now2, size, cache2), result)


writeCache :: Cache -> Key -> ByteString -> IO ()
writeCache ref key value =
  atomicModifyIORef' ref (\x -> (writeCache_ key value x, ()))

writeCache_
  :: Key
  -> ByteString
  -> (Natural, Natural, PSQ Key Natural ByteString)
  -> (Natural, Natural, PSQ Key Natural ByteString)
writeCache_ key value (now, size, cache) =
  let
    -- First, optimistically insert, even if we are at capacity (because we
    -- might just replace an existing entry, which is ok).
    size2 :: Natural
    cache2 :: PSQ Key Natural ByteString
    (size2, cache2) =
      PSQ.alter
        (\case
          Nothing -> (size+1, Just (now, value))
          Just _  -> (size,   Just (now, value)))
        key
        cache

    -- Then, if we went past capacity, delete the minimum.
    size3 :: Natural
    cache3 :: PSQ Key Natural ByteString
    (size3, cache3) =
      if size2 > capacity then -- if >, should always be == capacity+1
        (size, PSQ.deleteMin cache2)
      else
        (size2, cache2)

    now2 :: Natural
    now2 =
      now + 1
  in
    now2 `seq` size3 `seq` (now2, size3, cache3)
