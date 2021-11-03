module Cache where

import Prelude

import Control.Monad.State (class MonadState)
import Control.Monad.State as MonadState
import Data.Lens (Lens', over, view)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Type.Prelude (Proxy(..))

-- Cache that can get and set its value in a Monad and peek (with default) for a obtaining a pure value.

-- The cache object type parameter is consistently placed at the end of the type parameter list,
-- such that newtype instances can be derived.

-- The functional dependency "c -> a" promises that we won't declare multiple instances with the same "c" type.
class CacheEntry a c | c -> a where
  getCacheValue :: c -> Maybe a
  setCacheValue :: c -> Maybe a -> c

class CacheDefault a c | c -> a where
  default :: c -> a

class Monad m <= Fetchable m a c | c -> a where
  fetch :: c -> m (Maybe a)

class Monad m <= Flushable m a c | c -> a where
  flush :: c -> a -> m Unit

class CacheInvalidator s c where
  invalidate :: c -> s -> s

noInvalidate :: forall c s . c -> s -> s
noInvalidate _ s = s

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

-- TODO: can we get rid of the proxy?
purge :: forall a s c . CacheEntry a c => CacheInvalidator s c => Proxy a -> Lens' s c -> s -> s
purge _ _key state = (over _key (\c -> setCacheValue c (Nothing :: Maybe a)) state) # invalidate cache
  where cache = view _key state

-- TODO: we would want to introduce overloads for CacheInvalidator caches and non-CacheInvalidator caches.
-- But we can only do that if we create a class with a get function which is implemented differently based on instance constraints.
set :: forall m a s c . CacheEntry a c => CacheInvalidator s c => CacheDefault a c => Flushable m a c
  => Lens' s c -> a -> s -> m s
set _key value state = do
  flush cache value
  newState <- pure $ over _key (\c -> setCacheValue c (Just value)) state
  pure $ invalidate cache newState
  where cache = view _key state

get :: forall m a s c . CacheEntry a c => CacheDefault a c => Fetchable m a c
  => Lens' s c -> s -> m (Tuple s a)
get _key state = do
  cache <- pure $ view _key state
  case getCacheValue cache of
    Just value -> pure (Tuple state value)
    Nothing -> do
      maybeValue <- fetch cache
      value <- pure $ fromMaybe (default cache) maybeValue
      newState <- pure $ over _key (\c -> setCacheValue c (Just value)) state
      pure (Tuple newState value)

purgeM :: forall m a s c . CacheEntry a c => CacheInvalidator s c => MonadState s m => Proxy a -> Lens' s c -> m Unit
purgeM proxy _key = do
  state <- MonadState.get
  newState <- pure $ purge proxy _key state
  MonadState.put newState

setM :: forall m a s c . CacheEntry a c => CacheInvalidator s c => CacheDefault a c => Flushable m a c => MonadState s m
  => Lens' s c -> a -> m Unit
setM _key value = do
  state <- MonadState.get
  newState <- set _key value state
  MonadState.put newState

getM :: forall m a s c . CacheEntry a c => CacheDefault a c => Fetchable m a c => MonadState s m
  => Lens' s c -> m a
getM _key = do
  state <- MonadState.get
  cache <- pure $ view _key state
  case getCacheValue cache of
    Just value -> pure value
    Nothing -> do
      maybeValue <- fetch cache
      value <- pure $ fromMaybe (default cache) maybeValue
      newState <- pure $ over _key (\c -> setCacheValue c (Just value)) state
      MonadState.put newState
      pure value
