module CacheTests where

import Prelude

import Cache
import Data.Lens (Lens')
import Data.Lens.Barlow (barlow, key)
import Data.Lens.Barlow.Helpers (view, over)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Test.Assert (assert')
import Test.QuickCheck (Result(..), quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Data.Newtype (class Newtype)

main :: Effect Unit
main = do
  pure unit
--   initialState <- pure createTestState
--   Tuple newState (value::String) <- get _testValue initialState
--   newState <- set _testValue "blabla" newState
--   (peekedValue :: String) <- pure $ peek _testValue newState
--   pure unit

-- _testValue :: Lens' TestState MyCachedValue
-- _testValue = barlow (key :: _ "!.testValue")

-- newtype MyCachedValue = MyCachedValue (Maybe String)

-- newtype TestState = TestState
--   { testValue :: MyCachedValue }
-- derive instance Newtype TestState _

-- createTestState :: TestState
-- createTestState = TestState { testValue: MyCachedValue Nothing }

-- instance CacheEntry String MyCachedValue where
--   getCacheValue (MyCachedValue c) = c
--   setCacheValue (MyCachedValue c) newValue = MyCachedValue newValue
-- instance CacheDefault String MyCachedValue where
--   default _ = ""

-- instance CacheStore Effect String MyCachedValue where
--   fetch (MyCachedValue c) = pure $ Just "fetched value"
--   flush (MyCachedValue c) = \_ -> pure unit

-- instance CacheInvalidator TestState MyCachedValue where
--   invalidate cache state = state
