module Cache where

import Prelude

import Control.Monad.State (class MonadState)
import Control.Monad.State as MonadState
import Data.Exists (Exists, runExists)
import Data.Lens (Lens', over, set, view)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), snd)
import Type.Prelude (Proxy)
import Utils (foreach, foreach', foreachM)

-- Cache that can get and write its value in a Monad and peek (with default) for a obtaining a pure value.

-- The cache object type parameter is consistently placed at the end of the type parameter list,
-- such that newtype instances can be derived.

-- TODO: this cache implementation is now completely decentralized
-- However, we do want some form of central bookkeeping, because we want:
--   - A way to fetch everything (such that peeks will always return something e.g. in the rendering)
--   - A way to reverse/obsolete the invalidation functions. A Cache should be able to tell what other values it depends on.
--     Then when these dependencies change, the cache value should be invalidated by the bookkeeper.

-- TODO: make this an explicit type and store dependencies with it on a subscription basis?
-- Dependencies would subscribe during fetch.
-- The subcription list would be cleared after invalidation, such that a subsequent fetch won't result in duplicate subscriptions.
data CacheValue a = Cached a | NoValue
type CacheDependants s = List (AnyCacheKey s)

-- We need to wrap these functions in a datatype, otherwise the cache unit and containing records cannot occur in instance declarations.
data Fetch s a = Fetch (forall m . MonadState s m => m (Maybe a))
unFetch :: forall s a . Fetch s a -> forall m . MonadState s m => m (Maybe a)
unFetch (Fetch fetch) = fetch
data Flush a = Flush (forall m . Monad m => a -> m Unit)
unFlush :: forall a . Flush a -> forall m . Monad m => a -> m Unit
unFlush (Flush flush) = flush

type CacheUnit s a =
  { value :: CacheValue a
  , dependants :: CacheDependants s
  , fetch :: Fetch s a
  , flush :: Flush a
  , default :: a }
type CacheKey s a = Lens' s (CacheUnit s a)

newtype AnyCacheValue = AnyCacheValue (forall r. (forall a. CacheValue a -> r) -> r)
packCacheValue :: forall a. CacheValue a -> AnyCacheValue
packCacheValue x = AnyCacheValue \f -> f x
mapCacheValue :: forall r . (forall a . CacheValue a -> r) -> AnyCacheValue -> r
mapCacheValue f (AnyCacheValue y) = y f

newtype AnyCacheKey s = AnyCacheKey (forall r. (forall a. CacheKey s a -> r) -> r)
packCacheKey :: forall s a. CacheKey s a -> AnyCacheKey s
packCacheKey x = AnyCacheKey \f -> f x
mapAnyCacheKey :: forall r s. (forall a . CacheKey s a -> r) -> AnyCacheKey s -> r
mapAnyCacheKey f (AnyCacheKey y) = y f


-- Peek without default
peek' :: forall s a . CacheKey s a -> s -> CacheValue a
peek' _key state = cache.value
  where 
  cache :: CacheUnit s a
  cache = view _key state

-- Peek with default
peek :: forall s a . CacheKey s a -> s -> a
peek _key state =
  case cache.value of
    NoValue -> cache.default
    Cached result -> result
  where 
  cache :: CacheUnit s a
  cache = view _key state

purge :: forall m s a . MonadState s m => CacheKey s a -> m Unit
purge _key = do
  initialState <- MonadState.get
  cacheUnit <- pure $ view _key initialState
  cache <- pure $ view _key initialState
  case cache.value of
    NoValue -> pure unit
    Cached _ -> do
      set _key (cache { value = NoValue }) initialState # MonadState.put
      purgeDependencies cacheUnit

purgeDependencies :: forall m s a. MonadState s m => CacheUnit s a -> m Unit
purgeDependencies cache = do
  foreachM (cache.dependants) loop
  where
  loop anyKey = mapAnyCacheKey (\_key -> purge _key) anyKey

depend :: forall m s a b . MonadState s m => CacheKey s a -> CacheKey s b -> m b
depend _key _dependencyKey = do
  dependencyValue <- read _dependencyKey
  addDependency _key (packCacheKey _dependencyKey)
  pure dependencyValue

addDependency :: forall m s a . MonadState s m => CacheKey s a -> AnyCacheKey s -> m Unit
addDependency _key dependencyKey = do
  MonadState.get
    <#> over _key (\(c) -> c { dependants = Cons dependencyKey c.dependants })
    >>= MonadState.put

write :: forall m a s . MonadState s m => CacheKey s a -> a -> m Unit
write _key value = do
  cacheUnit <- MonadState.get <#> view _key
  cacheRecord <- pure $ cacheUnit
  (unFlush cacheRecord.flush) value
  purgeDependencies cacheUnit
  MonadState.get
    <#> over _key (\(c) -> c { value = Cached value })
    >>= MonadState.put

read :: forall m a s . MonadState s m => CacheKey s a -> m a
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
