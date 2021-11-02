module Cache where

import Prelude

import Control.Monad.State (class MonadState)
import Control.Monad.State as MonadState
import Data.Lens (Lens', over, view)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Type.Prelude (Proxy(..))

-- Cache that can get and set its value in a Monad and peek (with default) for a obtaining a pure value.

class CacheEntry a c where
  getCacheValue :: c -> Maybe a
  setCacheValue :: c -> Maybe a -> c

class CacheDefault a c where
  default :: c -> a

class Monad m <= CacheStore m a c where
  fetch :: c -> m (Maybe a)
  flush :: c -> a -> m Unit

class CacheInvalidator s c where
  invalidate :: c -> s -> s

noInvalidate :: forall c s . c -> s -> s
noInvalidate _ s = s

class (CacheEntry a c, CacheDefault a c, CacheStore m a c, CacheInvalidator s c) <= Cache m a s c
instance (CacheEntry a c, CacheDefault a c, CacheStore m a c, CacheInvalidator s c) => Cache m a s c

peek :: forall a s c . CacheEntry a c => CacheDefault a c => Lens' s c -> s -> a
peek _key state =
  case getCacheValue cache of
    Nothing -> default cache
    Just result -> result
  where
  cache = view _key state

purge :: forall a s c . CacheEntry a c => CacheInvalidator s c => Proxy a -> Lens' s c -> s -> s
purge _ _key state = (over _key (\c -> setCacheValue c (Nothing :: Maybe a)) state) # invalidate cache
  where cache = view _key state

set :: forall m a s c . Cache m a s c => Lens' s c -> a -> s -> m s
set _key value state = do
  flush cache value
  newState <- pure $ over _key (\c -> setCacheValue c (Just value)) state
  pure $ invalidate cache newState
  where cache = view _key state

get :: forall m a s c . Cache m a s c => Lens' s c -> s -> m (Tuple s a)
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

setM :: forall m a s c . Cache m a s c => MonadState s m => Lens' s c -> a -> m Unit
setM _key value = do
  state <- MonadState.get
  newState <- set _key value state
  MonadState.put newState

getM :: forall m a s c . Cache m a s c => MonadState s m => Lens' s c -> m a
getM _key = do
  state <- MonadState.get
  (Tuple newState value) <- get _key state
  MonadState.put newState
  pure value
