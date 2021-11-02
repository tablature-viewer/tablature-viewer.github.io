module Cache where

import Prelude

import Data.Lens (Lens', over, view)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (toUpper)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)

-- Cache that can get and set its value in a Monad and peek (with default) for a obtaining a pure value.

data Cache a = Cached a | NoValue

class Monad m <= FetchableString f m where
  fetchString :: f -> m String

getString :: forall f m . FetchableString f m => Monad m => f -> Cache String -> m String
getString fetchable cache =
  case cache of
    Cached value -> pure value
    NoValue -> do
      value <- fetchString fetchable
      pure $ value

newtype MyCachedString = MyCachedString (Cache String)



class Monad m <= Fetchable f m a where
  fetch :: f -> m a

get :: forall f m a. Fetchable f m a => Monad m => f -> Cache a -> m a
get fetchable cache =
  case cache of
    Cached value -> pure value
    NoValue -> do
      value <- fetch fetchable
      pure $ value

newtype MyCachedInt = MyCachedInt (Cache Int)



type TestState =
  { myCachedString :: MyCachedString
  , myCachedInt :: MyCachedInt }

createTestState :: TestState
createTestState =
  { myCachedString: MyCachedString NoValue
  , myCachedInt: MyCachedInt NoValue }

main :: Effect Unit
main = do
  state <- pure createTestState
  fetchable <- pure state.myCachedInt
  cache <- pure $ (\(MyCachedInt c) -> c) fetchable
  int <- get fetchable cache
  pure unit

instance Fetchable MyCachedInt Effect Int where
  fetch (MyCachedInt c) = do
    log "hi"
    pure 0

instance FetchableString MyCachedString Effect where
  fetchString (MyCachedString c) = do
    log "hi"
    pure "fetched value"
