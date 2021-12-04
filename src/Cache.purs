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

type CacheUnit s a =
  { value :: CacheValue a
  , default :: a
  , dependants :: CacheDependants s }

buildCache :: forall s a . a -> CacheUnit s a
buildCache default =
  { value: NoValue
  , default: default
  , dependants: Nil }

-- This needs to be a newtype because otherwise we get "Could not match constrained type" errors
newtype CacheKey s a = CacheKey (Lens' s (CacheUnit s a))
runCacheKey (CacheKey _key) = _key
viewCacheUnit key = view (runCacheKey key)
setCacheUnit key = set (runCacheKey key)
overCacheUnit key = over (runCacheKey key)

newtype AnyCacheKey s = AnyCacheKey (forall result. (forall a. CacheKey s a -> result) -> result)
packCacheKey :: forall s a. CacheKey s a -> AnyCacheKey s
packCacheKey x = AnyCacheKey \f -> f x
mapAnyCacheKey :: forall result s. (forall a . CacheKey s a -> result) -> AnyCacheKey s -> result
mapAnyCacheKey f (AnyCacheKey y) = y f

-- We need to wrap these functions in a datatype, otherwise the cache unit and containing records cannot occur in instance declarations.
data Fetch a m = Fetch (m (Maybe a))
runFetch :: forall a m . Fetch a m -> m (Maybe a)
runFetch (Fetch fetch) = fetch
data Flush a m = Flush (a -> m Unit)
runFlush :: forall a m . Flush a m -> a -> m Unit
runFlush (Flush flush) = flush

type CacheDef s a r =
  { cache :: CacheKey s a
  | r }


type ReadableCacheDef s a r m = CacheDef s a ( fetch :: Fetch a m | r )
type WritableCacheDef s a r m = CacheDef s a ( flush :: Flush a m | r )
type ReadWriteCacheDef s a r m = CacheDef s a ( flush :: Flush a m, fetch :: Fetch a m | r )

type ReadableCacheDefKey s a r m = Lens' s ( ReadableCacheDef s a r m )
type WritableCacheDefKey s a r m = Lens' s ( WritableCacheDef s a r m )
type ReadWriteCacheDefKey s a r m = Lens' s ( ReadWriteCacheDef s a r m )

-- type ReadonlyCacheDefBuilder a m = 
--   { cache :: CacheKey s a
--   , fetch :: Fetch a m
--   }

-- readonlyCache :: forall s a m . ReadonlyCacheBuilder a m -> ReadableCacheDef s a () m
-- readonlyCache builder = Record.merge builder
--   { value: NoValue :: CacheValue a
--   , dependants: Nil :: List (AnyCacheKey s)
--   }

-- type ReadWriteCacheBuilder a m = 
--   { default :: a
--   , fetch :: Fetch a m
--   , flush :: Flush a m
--   }

-- readWriteCache :: forall s a m . ReadWriteCacheBuilder a m -> ReadWriteCacheDef s a () m
-- readWriteCache builder = Record.merge builder
--   { value: NoValue :: CacheValue a
--   , dependants: Nil :: List (AnyCacheKey s)
--   }


newtype AnyCacheValue = AnyCacheValue (forall result. (forall a. CacheValue a -> result) -> result)
packCacheValue :: forall a. CacheValue a -> AnyCacheValue
packCacheValue x = AnyCacheValue \f -> f x
mapCacheValue :: forall result . (forall a . CacheValue a -> result) -> AnyCacheValue -> result
mapCacheValue f (AnyCacheValue y) = y f


-- Peek without default
peek' :: forall s a . CacheKey s a -> s -> CacheValue a
peek' _key state = cache.value
  where 
  cache :: CacheUnit s a 
  cache = viewCacheUnit _key state

-- Peek with default
peek :: forall s a . CacheKey s a -> s -> a
peek _key state =
  case cache.value of
    NoValue -> cache.default
    Cached result -> result
  where 
  cache :: CacheUnit s a 
  cache = viewCacheUnit _key state

purge :: forall m s a . MonadState s m => CacheKey s a -> m Unit
purge _key = do
  initialState <- MonadState.get
  cache <- pure $ viewCacheUnit _key initialState
  cache <- pure $ viewCacheUnit _key initialState
  case cache.value of
    NoValue -> pure unit
    Cached _ -> do
      setCacheUnit _key (cache { value = NoValue }) initialState # MonadState.put
      purgeDependencies cache

purgeDependencies :: forall m s a . MonadState s m => CacheUnit s a -> m Unit
purgeDependencies cache = do
  foreachM (cache.dependants) loop
  where
  loop anyKey = mapAnyCacheKey (\_key -> purge _key) anyKey

depend :: forall m s a b r2. MonadState s m => CacheKey s a -> ReadableCacheDef s b r2 m -> m b
depend _key dependency = do
  dependencyValue <- read dependency
  addDependency _key (packCacheKey dependency.cache)
  pure dependencyValue

addDependency :: forall m s a . MonadState s m => CacheKey s a -> AnyCacheKey s -> m Unit
addDependency _key dependencyKey = do
  MonadState.get
    <#> overCacheUnit _key (\(c) -> c { dependants = Cons dependencyKey c.dependants })
    >>= MonadState.put

write :: forall m a s r . MonadState s m => WritableCacheDef s a r m -> a -> m Unit
write def value = do
  cache <- MonadState.get <#> viewCacheUnit def.cache
  (runFlush def.flush) value
  purgeDependencies cache
  MonadState.get
    <#> overCacheUnit def.cache (\(c) -> c { value = Cached value })
    >>= MonadState.put

read :: forall m a s r . MonadState s m => ReadableCacheDef s a r m -> m a
read def = do
  cache <- MonadState.get <#> view (runCacheKey def.cache)
  case cache.value of
    Cached value -> pure value
    NoValue -> do
      maybeValue <- (runFetch def.fetch)
      value <- pure $ fromMaybe (cache.default) maybeValue
      newState <- MonadState.get <#> over (runCacheKey def.cache) (\(c) -> c { value = Cached value })
      MonadState.put newState
      pure value
