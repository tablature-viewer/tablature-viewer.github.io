module Cache where

import Prelude

import Control.Monad.State (class MonadState)
import Control.Monad.State as MonadState
import Data.Lens (Lens', over, set, view)
import Data.List (List(..))
import Data.Maybe (Maybe, fromMaybe)
import Utils (foreachM)

-- Cached value that can be read and written in a State Monad and be "peeked" (with default) in a pure context

-- TODO: this cache implementation is now completely decentralized
-- However, we do want some form of central bookkeeping, because we want:
--   - A way to fetch everything (such that peeks will always return something e.g. in the rendering)

-- Dependencies subscribe during fetch.
-- The subcription list is be cleared after invalidation, such that a subsequent fetch won't result in duplicate subscriptions.
data CacheValue a = Cached a | NoValue
type CacheDependants s = List (AnyEntryKey s)

type CacheEntry s a =
  { value :: CacheValue a
  , default :: a
  , dependants :: CacheDependants s }

buildCache :: forall s a . a -> CacheEntry s a
buildCache default =
  { value: NoValue
  , default: default
  , dependants: Nil }

-- This needs to be a newtype because otherwise we get "Could not match constrained type" errors
newtype EntryKey s a = EntryKey (Lens' s (CacheEntry s a))
runEntryKey :: forall s a . EntryKey s a -> Lens' s (CacheEntry s a)
runEntryKey (EntryKey _key) = _key
viewEntry :: forall s a . EntryKey s a -> s -> CacheEntry s a
viewEntry key = view (runEntryKey key)
setEntry :: forall s a . EntryKey s a -> CacheEntry s a -> s -> s
setEntry key = set (runEntryKey key)
overEntry :: forall s a . EntryKey s a -> (CacheEntry s a -> CacheEntry s a) -> s -> s
overEntry key = over (runEntryKey key)

newtype AnyEntryKey s = AnyEntryKey (forall result. (forall a. EntryKey s a -> result) -> result)
mkAnyEntryKey :: forall s a. EntryKey s a -> AnyEntryKey s
mkAnyEntryKey x = AnyEntryKey \f -> f x
mapAnyEntryKey :: forall result s. (forall a . EntryKey s a -> result) -> AnyEntryKey s -> result
mapAnyEntryKey f (AnyEntryKey y) = y f

-- We need to wrap these functions in a datatype, otherwise the cache unit and containing records cannot occur in instance declarations.
data Fetch a m = Fetch (m (Maybe a))
runFetch :: forall a m . Fetch a m -> m (Maybe a)
runFetch (Fetch fetch) = fetch
data Flush a m = Flush (a -> m Unit)
runFlush :: forall a m . Flush a m -> a -> m Unit
runFlush (Flush flush) = flush

type CacheUnit s a r =
  { entry :: EntryKey s a
  | r }

type ReadableCacheUnit s a r m = CacheUnit s a ( fetch :: Fetch a m | r )
type WritableCacheUnit s a r m = CacheUnit s a ( flush :: Flush a m | r )
type ReadWriteCacheUnit s a r m = CacheUnit s a ( flush :: Flush a m, fetch :: Fetch a m | r )


-- Peek without default
peek' :: forall s a . EntryKey s a -> s -> CacheValue a
peek' _key state = entry.value
  where 
  entry = viewEntry _key state

-- Peek with default
peek :: forall s a . EntryKey s a -> s -> a
peek _key state =
  case entry.value of
    NoValue -> entry.default
    Cached result -> result
  where 
  entry = viewEntry _key state

purge :: forall m s a . MonadState s m => EntryKey s a -> m Unit
purge _key = do
  initialState <- MonadState.get
  entry <- pure $ viewEntry _key initialState
  case entry.value of
    NoValue -> pure unit
    Cached _ -> do
      setEntry _key (entry { value = NoValue }) initialState # MonadState.put
      purgeDependencies entry

purgeDependencies :: forall m s a . MonadState s m => CacheEntry s a -> m Unit
purgeDependencies entry = do
  foreachM entry.dependants loop
  where
  loop anyKey = mapAnyEntryKey (\_key -> purge _key) anyKey

depend :: forall m s a b r. MonadState s m => EntryKey s a -> ReadableCacheUnit s b r m -> m b
depend _key dependency = do
  dependencyValue <- read dependency
  addDependency _key (mkAnyEntryKey dependency.entry)
  pure dependencyValue

addDependency :: forall m s a . MonadState s m => EntryKey s a -> AnyEntryKey s -> m Unit
addDependency _key dependencyKey = do
  MonadState.get
    <#> overEntry _key (\entry -> entry { dependants = Cons dependencyKey entry.dependants })
    >>= MonadState.put

write :: forall m a s r . MonadState s m => WritableCacheUnit s a r m -> a -> m Unit
write cache value = do
  entry <- MonadState.get <#> viewEntry cache.entry
  runFlush cache.flush value
  purgeDependencies entry
  MonadState.get
    <#> overEntry cache.entry _ { value = Cached value }
    >>= MonadState.put

read :: forall m a s r . MonadState s m => ReadableCacheUnit s a r m -> m a
read cache = do
  entry <- MonadState.get <#> viewEntry cache.entry
  case entry.value of
    Cached value -> pure value
    NoValue -> do
      maybeValue <- runFetch cache.fetch
      value <- pure $ fromMaybe entry.default maybeValue
      newState <- MonadState.get <#> overEntry cache.entry _ { value = Cached value }
      MonadState.put newState
      pure value
