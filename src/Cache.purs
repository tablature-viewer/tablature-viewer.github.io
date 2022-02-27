module Cache where

import Prelude

import Control.Monad.State (class MonadState)
import Control.Monad.State as MonadState
import Data.Lens (Lens', over, view)
import Data.List (List(..))
import Data.Maybe (Maybe, fromMaybe)
import Utils (foreachM)

-- Cached value that can be read and written in a State Monad and be "peeked" (with default) in a pure context
-- Dependants can subscribe to dependencies during fetch.
-- The subcription list is to be cleared after invalidation, such that a subsequent fetch won't result in duplicate subscriptions.

-- TODO: this cache implementation is now completely decentralized
-- However, we do want some form of central bookkeeping, because we want:
--   - A way to fetch everything (such that peeks will always return something e.g. in the rendering)

data CacheValue a = Cached a | NoValue
type CacheEntry s a =
  { value :: CacheValue a
  , default :: a
  , dependants :: List (AnyEntryKey s)
  }

newtype AnyEntryKey s = AnyEntryKey (forall result. (forall a. EntryKey s a -> result) -> result)

mkAnyEntryKey :: forall s a. EntryKey s a -> AnyEntryKey s
mkAnyEntryKey x = AnyEntryKey \f -> f x

mapAnyEntryKey :: forall result s. (forall a. EntryKey s a -> result) -> AnyEntryKey s -> result
mapAnyEntryKey f (AnyEntryKey y) = y f

buildCache :: forall s a. a -> CacheEntry s a
buildCache default =
  { value: NoValue
  , default: default
  , dependants: Nil
  }

-- This needs to be a newtype because otherwise we get "Could not match constrained type" errors
newtype EntryKey s a = EntryKey (Lens' s (CacheEntry s a))

runEntryKey :: forall s a. EntryKey s a -> Lens' s (CacheEntry s a)
runEntryKey (EntryKey _key) = _key

viewEntry' :: forall s a. EntryKey s a -> s -> CacheEntry s a
viewEntry' key = view (runEntryKey key)

viewEntry :: forall s a m. MonadState s m => EntryKey s a -> m (CacheEntry s a)
viewEntry key = MonadState.get <#> viewEntry' key

overEntry :: forall s a m. MonadState s m => EntryKey s a -> (CacheEntry s a -> CacheEntry s a) -> m Unit
overEntry key f = MonadState.get <#> over (runEntryKey key) f >>= MonadState.put

-- We need to wrap these functions in a datatype, otherwise the cache unit and containing records cannot occur in instance declarations.
data Fetch a m = Fetch (m (Maybe a))

runFetch :: forall a m. Fetch a m -> m (Maybe a)
runFetch (Fetch fetch) = fetch

data Flush a m = Flush (a -> m Unit)

runFlush :: forall a m. Flush a m -> a -> m Unit
runFlush (Flush flush) = flush

type CacheUnit s a r =
  { entry :: EntryKey s a
  | r
  }

type ReadWriteCacheUnit s a r m = CacheUnit s a (flush :: Flush a m, fetch :: Fetch a m | r)
type WritableCacheUnit s a r m = CacheUnit s a (flush :: Flush a m | r)
type ReadableCacheUnit s a r m = CacheUnit s a (fetch :: Fetch a m | r)

-- Peek without default
peek' :: forall s a. EntryKey s a -> s -> CacheValue a
peek' _key state = entry.value
  where
  entry = viewEntry' _key state

-- Peek with default
peek :: forall s a. EntryKey s a -> s -> a
peek _key state =
  case entry.value of
    NoValue -> entry.default
    Cached result -> result
  where
  entry = viewEntry' _key state

invalidate :: forall m s a. MonadState s m => EntryKey s a -> m Unit
invalidate _key = do
  entry <- viewEntry _key
  case entry.value of
    NoValue -> pure unit
    Cached _ -> do
      overEntry _key _ { value = NoValue, dependants = Nil }
      invalidateDependants entry

invalidateDependants :: forall m s a. MonadState s m => CacheEntry s a -> m Unit
invalidateDependants entry = foreachM entry.dependants $ mapAnyEntryKey invalidate

subscribe :: forall m s a b r. MonadState s m => EntryKey s a -> ReadableCacheUnit s b r m -> m b
subscribe _dependant dependency = do
  dependencyValue <- read dependency
  addDependant dependency _dependant
  pure dependencyValue

addDependant :: forall m s a b r. MonadState s m => CacheUnit s b r -> EntryKey s a -> m Unit
addDependant dependency _dependant =
  overEntry dependency.entry (\entry -> entry { dependants = Cons (mkAnyEntryKey _dependant) entry.dependants })

write :: forall m a s r. MonadState s m => WritableCacheUnit s a r m -> a -> m Unit
write cache value = do
  entry <- viewEntry cache.entry
  runFlush cache.flush value
  invalidateDependants entry
  overEntry cache.entry _ { value = Cached value }

read :: forall m a s r. MonadState s m => ReadableCacheUnit s a r m -> m a
read cache = do
  entry <- viewEntry cache.entry
  case entry.value of
    Cached value -> pure value
    NoValue -> do
      maybeValue <- runFetch cache.fetch
      value <- pure $ fromMaybe entry.default maybeValue
      overEntry cache.entry _ { value = Cached value }
      pure value
