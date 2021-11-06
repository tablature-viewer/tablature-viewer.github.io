module Cache where

import Prelude

import Control.Monad.State (class MonadState)
import Control.Monad.State as MonadState
import Data.Exists (Exists, runExists)
import Data.Lens (Lens', over, set, view)
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), snd)
import Type.Prelude (Proxy)
import Utils (foreach, foreach')

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
class CacheEntry a c | c -> a where
  getCacheValue :: c -> Maybe a
  setCacheValue :: c -> a -> c

class Purgeable c where
  purgeCacheValue :: c -> c
  hasCacheValue :: c -> Boolean

newtype AnyPurgeableLens s = AnyPurgeableLens (forall r. (forall x. Purgeable x => Dependable s x => Lens' s x -> r) -> r)

packPurgeableLens :: forall x s. Purgeable x => Dependable s x => Lens' s x -> AnyPurgeableLens s
packPurgeableLens x = AnyPurgeableLens \f -> f x

mapPurgableLens :: forall r s. (forall x. Purgeable x => Dependable s x => Lens' s x -> r) -> AnyPurgeableLens s -> r
mapPurgableLens f (AnyPurgeableLens y) = y f

class Dependable s c where
  dependants :: c -> List (AnyPurgeableLens s)

-- The functional dependency "c -> a" enforces that we won't declare multiple instances with the same "c" type.
class CacheDefault a c | c -> a where
  default :: c -> a

class Monad m <= Fetchable m a c | c -> a where
  fetch :: c -> m (Maybe a)

class Monad m <= Flushable m a c | c -> a where
  flush :: c -> a -> m Unit

-- Peek without default
peek' :: forall a s c . CacheEntry a c => Lens' s c -> s -> Maybe a
peek' _key state = view _key state # getCacheValue

-- Peek with default
peek :: forall a s c . CacheEntry a c => CacheDefault a c => Lens' s c -> s -> a
peek _key state =
  case getCacheValue cache of
    Nothing -> default cache
    Just result -> result
  where
  cache = view _key state

purge :: forall s c . Purgeable c => Dependable s c => Lens' s c -> s -> s
purge _key state =
  if not hasCacheValue cache
  then state
  else set _key (purgeCacheValue cache) state # purgeDependencies cache
  where cache = view _key state

purgeDependencies :: forall s c. Dependable s c => c -> s -> s
purgeDependencies cache initialState = foreach' initialState (dependants cache) loop
  where
  loop state packed = mapPurgableLens (\lens -> purge lens state) packed

write :: forall m a s c . CacheEntry a c => Dependable s c => CacheDefault a c => Flushable m a c =>
  Lens' s c -> a -> s -> m s
write _key value state = do
  flush cache value
  newState <- pure $ over _key (\c -> setCacheValue c value) state
  pure $ purgeDependencies cache newState
  where cache = view _key state

purgeM :: forall m s c . Purgeable c => Dependable s c => MonadState s m => Lens' s c -> m Unit
purgeM _key = MonadState.get <#> purge _key >>= MonadState.put

writeM :: forall m a s c . CacheEntry a c => Dependable s c => CacheDefault a c => Flushable m a c => MonadState s m
  => Lens' s c -> a -> m Unit
writeM _key value = MonadState.get >>= write _key value >>= MonadState.put

readM :: forall m a s c . CacheEntry a c => CacheDefault a c => Fetchable m a c => MonadState s m => Lens' s c -> m a
readM _key = do
  cache <- MonadState.get <#> view _key
  case getCacheValue cache of
    Just value -> pure value
    Nothing -> do
      maybeValue <- fetch cache
      value <- pure $ fromMaybe (default cache) maybeValue
      newState <- MonadState.get <#> over _key (\c -> setCacheValue c value)
      MonadState.put newState
      pure value
