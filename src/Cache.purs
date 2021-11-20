module Cache where

import Prelude

import Control.Monad.State (class MonadState)
import Control.Monad.State as MonadState
import Data.Lens (Lens', over, set, view)
import Data.List (List(..))
import Data.Maybe (Maybe, fromMaybe)
import Record as Record
import Utils (foreachM)

-- Cached value that can be read and written in a State Monad and be "peeked" (with default) in a pure context

-- TODO: this cache implementation is now completely decentralized
-- However, we do want some form of central bookkeeping, because we want:
--   - A way to fetch everything (such that peeks will always return something e.g. in the rendering)
--   - A way to reverse/obsolete the invalidation functions. A Cache should be able to tell what other values it depends on.
--     Then when these dependencies change, the cache value should be invalidated by the bookkeeper.

-- Dependencies subscribe during fetch.
-- The subcription list is be cleared after invalidation, such that a subsequent fetch won't result in duplicate subscriptions.
data CacheValue a = Cached a | NoValue
type CacheDependants s = List (AnyCacheKey s)

-- We need to wrap these functions in a datatype, otherwise the cache unit and containing records cannot occur in instance declarations.
data Fetch s a = Fetch (forall m . MonadState s m => m (Maybe a))
unFetch :: forall s a . Fetch s a -> forall m . MonadState s m => m (Maybe a)
unFetch (Fetch fetch) = fetch
data Flush a = Flush (forall m . Monad m => a -> m Unit)
unFlush :: forall a . Flush a -> forall m . Monad m => a -> m Unit
unFlush (Flush flush) = flush

type CacheUnit s a r =
  { value :: CacheValue a
  , default :: a
  , dependants :: CacheDependants s
  | r }
type ReadableCacheUnit s a r = CacheUnit s a ( fetch :: Fetch s a | r )
type WritableCacheUnit s a r = CacheUnit s a ( flush :: Flush a | r )
type ReadWriteCacheUnit s a r = CacheUnit s a ( flush :: Flush a, fetch :: Fetch s a | r )

type CacheKey s a r = Lens' s (CacheUnit s a r)
type ReadableCacheKey s a r = Lens' s (ReadableCacheUnit s a r)
type WritableCacheKey s a r = Lens' s (WritableCacheUnit s a r)
type RWCacheKey s a r = Lens' s (ReadWriteCacheUnit s a r)

type ReadonlyCacheBuilder s a = 
  { default :: a
  , fetch :: Fetch s a
  }

readonlyCache :: forall s a . ReadonlyCacheBuilder s a -> ReadableCacheUnit s a ()
readonlyCache builder = Record.merge builder
  { value: NoValue :: CacheValue a
  , dependants: Nil :: List (AnyCacheKey s)
  }

type ReadWriteCacheBuilder s a = 
  { default :: a
  , fetch :: Fetch s a
  , flush :: Flush a
  }

readWriteCache :: forall s a . ReadWriteCacheBuilder s a -> ReadWriteCacheUnit s a ()
readWriteCache builder = Record.merge builder
  { value: NoValue :: CacheValue a
  , dependants: Nil :: List (AnyCacheKey s)
  }


newtype AnyCacheValue = AnyCacheValue (forall result. (forall a. CacheValue a -> result) -> result)
packCacheValue :: forall a. CacheValue a -> AnyCacheValue
packCacheValue x = AnyCacheValue \f -> f x
mapCacheValue :: forall result . (forall a . CacheValue a -> result) -> AnyCacheValue -> result
mapCacheValue f (AnyCacheValue y) = y f

newtype AnyCacheKey s = AnyCacheKey (forall result. (forall a r. CacheKey s a r -> result) -> result)
packCacheKey :: forall s a r. CacheKey s a r -> AnyCacheKey s
packCacheKey x = AnyCacheKey \f -> f x
mapAnyCacheKey :: forall result s. (forall a r . CacheKey s a r -> result) -> AnyCacheKey s -> result
mapAnyCacheKey f (AnyCacheKey y) = y f


-- Peek without default
peek' :: forall s a r. CacheKey s a r -> s -> CacheValue a
peek' _key state = cache.value
  where 
  cache :: CacheUnit s a r
  cache = view _key state

-- Peek with default
peek :: forall s a r. CacheKey s a r -> s -> a
peek _key state =
  case cache.value of
    NoValue -> cache.default
    Cached result -> result
  where 
  cache :: CacheUnit s a r
  cache = view _key state

purge :: forall m s a r. MonadState s m => CacheKey s a r -> m Unit
purge _key = do
  initialState <- MonadState.get
  cacheUnit <- pure $ view _key initialState
  cache <- pure $ view _key initialState
  case cache.value of
    NoValue -> pure unit
    Cached _ -> do
      set _key (cache { value = NoValue }) initialState # MonadState.put
      purgeDependencies cacheUnit

purgeDependencies :: forall m s a r. MonadState s m => CacheUnit s a r -> m Unit
purgeDependencies cache = do
  foreachM (cache.dependants) loop
  where
  loop anyKey = mapAnyCacheKey (\_key -> purge _key) anyKey

depend :: forall m s a b r1 r2. MonadState s m => CacheKey s a r1 -> ReadableCacheKey s b r2 -> m b
depend _key _dependencyKey = do
  dependencyValue <- read _dependencyKey
  addDependency _key (packCacheKey _dependencyKey)
  pure dependencyValue

addDependency :: forall m s a r . MonadState s m => CacheKey s a r -> AnyCacheKey s -> m Unit
addDependency _key dependencyKey = do
  MonadState.get
    <#> over _key (\(c) -> c { dependants = Cons dependencyKey c.dependants })
    >>= MonadState.put

write :: forall m a s r . MonadState s m => WritableCacheKey s a r -> a -> m Unit
write _key value = do
  cacheUnit <- MonadState.get <#> view _key
  cacheRecord <- pure $ cacheUnit
  (unFlush cacheRecord.flush) value
  purgeDependencies cacheUnit
  MonadState.get
    <#> over _key (\(c) -> c { value = Cached value })
    >>= MonadState.put

read :: forall m a s r . MonadState s m => ReadableCacheKey s a r -> m a
read _key = do
  cache <- MonadState.get <#> view _key
  case cache.value of
    Cached value -> pure value
    NoValue -> do
      maybeValue <- (unFetch cache.fetch)
      value <- pure $ fromMaybe (cache.default) maybeValue
      newState <- MonadState.get <#> over _key (\(c) -> c { value = Cached value })
      MonadState.put newState
      pure value
