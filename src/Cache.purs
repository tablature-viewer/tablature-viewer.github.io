module Cache where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Lens (Lens', over, view)
import Data.Maybe (Maybe, fromMaybe)

-- Cache that can get and set its value in a Monad and peek (with default) for a obtaining a pure value.

data CacheValue a = NoValue | Cached a

class Cache c where
  fetch :: forall m a . c m a -> m (Maybe a)
  flush :: forall m a . c m a -> a -> m Unit
  getCacheValue :: forall m a . c m a -> CacheValue a
  setCacheValue :: forall m a . c m a -> CacheValue a -> c m a
  default :: forall m a . c m a -> a


peek :: forall c s m a. Cache c => Lens' s (c m a) -> s -> a
peek _key state =
  case getCacheValue cache of
    NoValue -> default cache
    Cached result -> result
  where
  cache = view _key state

purge :: forall c s m a. Cache c => Lens' s (c m a) -> s -> s
purge _key state = over _key (\c -> setCacheValue c NoValue) state

set :: forall c s m a. Cache c => Monad m => Lens' s (c m a) -> a -> s -> m s
set _key value state = do
  flush (view _key state) value
  pure $ over _key (\c -> setCacheValue c (Cached value)) state

get :: forall c s m a. Cache c => Monad m => Lens' s (c m a) -> s -> m (Tuple s a)
get _key state = do
  cache <- pure $ view _key state
  case getCacheValue cache of
    Cached value -> pure (Tuple state value)
    NoValue -> do
      maybeValue <- fetch cache
      value <- pure $ fromMaybe (default cache) maybeValue
      newState <- pure $ over _key (\c -> setCacheValue c (Cached value)) state
      pure (Tuple newState value)


newtype CacheRecord m a = CacheRecord
  { fetch :: m (Maybe a)
  , flush :: a -> m Unit
  , cacheValue :: CacheValue a
  , default :: a
  }

instance Cache CacheRecord where
  fetch (CacheRecord c) = c.fetch
  flush (CacheRecord c) = c.flush
  getCacheValue (CacheRecord c) = c.cacheValue
  setCacheValue (CacheRecord c) newValue = CacheRecord c { cacheValue = newValue }
  default (CacheRecord c) = c.default

type CacheBuilder m a =
  { fetch :: m (Maybe a)
  , flush :: a -> m Unit
  , default :: a
  }

create :: forall m a . CacheBuilder m a -> CacheRecord m a
create builder = CacheRecord
  { fetch: builder.fetch
  , flush: builder.flush
  , cacheValue: NoValue
  , default: builder.default
  }
