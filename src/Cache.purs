module Cache where

import Prelude

import Control.Monad.State (class MonadState)
import Control.Monad.State.Class as MonadState
import Data.Lens (Lens', over, view)
import Data.Maybe (Maybe, fromMaybe)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Type.Prelude (Proxy(..))

-- type Stuff =
--   { foo :: forall a. a -> a
--   , bar :: forall a. Eq a => a -> a
--   , baz :: forall a. Eq a => String -> a -> a
--   }

-- testbar :: forall a. Eq a => Show a => a -> a
-- testbar a = a

-- test =
--   { foo: \a -> a
--   , bar: testbar
--   , baz: \s a -> a
--   }


-- v1 :: Stuff -> Int
-- v1 r =
--   r.baz "thing" $ r.foo 1 -- works if function takes more than one arg
  
-- v2 :: Stuff -> Int
-- v2 r =
--   r.bar $ r.foo 1 -- fails to compile

-- Enables defining cached values in a MonadState
data CacheValue a = NoValue | Cached a

-- def :: forall s m a. MonadState s m => Proxy m -> Cache s a -> a
-- def p cache = cache.default p

-- get :: forall s m a . MonadState s m => Cache s a -> m a
-- get cache = cache.fetch 

{-}
type Cache s a = forall m. MonadState s m => 
  { fetch :: m a
  , default :: Proxy m -> a
  }

testFetch :: forall s m . MonadState s m => m Boolean
testFetch = pure true

testCache :: forall s m . MonadState s m => Cache s Boolean
testCache =
  { fetch: testFetch
  , default: \_ -> false
  }

testFetch' :: forall s m . MonadEffect m => MonadState s m => m Boolean
testFetch' = do
  liftEffect $ Console.log "hi"
  pure true

testCache' :: forall s m . MonadEffect m => MonadState s m => Cache s Boolean
testCache' =
  { fetch: testFetch'
  , default: \_ -> false
  }


type Fetch s a = forall m. MonadState s m => m a

data Cache' s a =  Cache' (Fetch s a) a

def' :: forall s a. Cache' s a -> a
def' (Cache' _ d) = d

fetch' :: forall s m a. MonadState s m => Cache' s a -> m a
fetch' (Cache' f _) = f

data Cache s a = Cache
  (forall m. MonadState s m => { fetch :: m a })
  { default :: a }

def :: forall s m a. MonadState s m => Cache s a -> a
def (Cache f d) = d.default

get :: forall s m a . MonadState s m => Cache s a -> m a
get (Cache f d) = f.fetch


newtype Cache s a = Cache
  { fetch :: Fetch s a
  , default :: a
  }

def :: forall s m a. MonadState s m => Cache s a -> a
def (Cache cache) = cache.default

get :: forall s m a . MonadState s m => Cache s a -> m a
get (Cache cache) = cache.fetch

-}


class Cache c where
  fetch :: forall m a . c m a -> m (Maybe a)
  flush :: forall m a . c m a -> a -> m Unit
  invalidate :: forall m a . c m a -> m Unit
  getCacheValue :: forall m a . c m a -> CacheValue a
  setCacheValue :: forall m a . c m a -> CacheValue a -> c m a
  default :: forall m a . c m a -> a


peekOrDefault :: forall a. a -> CacheValue a -> a
peekOrDefault default cacheValue = case cacheValue of
  NoValue -> default
  Cached result -> result

peek :: forall c s m a. Cache c => Lens' s (c m a) -> s -> a
peek _key state = peekOrDefault (default cache) (getCacheValue cache)
  where
  cache = view _key state

purge :: forall c s m a. Cache c => MonadState s m => Lens' s (c m a) -> m Unit
purge _key = do
  cache <- MonadState.get <#> view _key
  MonadState.modify_ (\state -> over _key (\c -> setCacheValue c NoValue) state)
  invalidate cache

set :: forall c s m a. Cache c => MonadState s m => Lens' s (c m a) -> a -> m Unit
set _key value = do
  cache <- MonadState.get <#> view _key
  MonadState.modify_ (\state -> over _key (\c -> setCacheValue c (Cached value)) state)
  flush cache value
  invalidate cache

get :: forall c s m a. Cache c => MonadState s m => Lens' s (c m a) -> m a
get _key = do
  cache <- MonadState.get <#> view _key
  case getCacheValue cache of
    Cached value -> pure value
    NoValue -> do
      value <- fetch cache <#> fromMaybe (default cache)
      MonadState.modify_ (\state -> over _key (\c -> setCacheValue c (Cached value)) state)
      pure value


newtype CacheRecord m a = CacheRecord
  { fetch :: m (Maybe a)
  , flush :: a -> m Unit
  , invalidate :: m Unit
  , cacheValue :: CacheValue a
  , default :: a
  }

instance Cache CacheRecord where
  fetch (CacheRecord c) = c.fetch
  flush (CacheRecord c) = c.flush
  invalidate (CacheRecord c) = c.invalidate
  getCacheValue (CacheRecord c) = c.cacheValue
  setCacheValue (CacheRecord c) newValue = CacheRecord c { cacheValue = newValue }
  default (CacheRecord c) = c.default

type CacheBuilder m a =
  { fetch :: m (Maybe a)
  , flush :: a -> m Unit
  , invalidate :: m Unit
  , default :: a
  }

create :: forall m a . CacheBuilder m a -> CacheRecord m a
create builder = CacheRecord
  { fetch: builder.fetch
  , flush: builder.flush
  , invalidate: builder.invalidate
  , cacheValue: NoValue
  , default: builder.default
  }
