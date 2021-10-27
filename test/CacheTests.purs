module CacheTests where

import Prelude

import Cache (CacheRecord(..), create, get, peek, set)
import Data.Lens (Lens')
import Data.Lens.Barlow (barlow, key)
import Data.Lens.Barlow.Helpers (view, over)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Test.Assert (assert')
import Test.QuickCheck (Result(..), quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary)

main :: Effect Unit
main = do
  initialState <- pure createTestState
  Tuple newState value <- get _testValue initialState
  newState <- set _testValue "blabla" newState
  peekedValue <- pure $ peek _testValue newState
  pure unit

_testValue :: Lens' TestState (CacheRecord Effect String)
_testValue = barlow (key :: _ ".testValue")

type TestState =
  { testValue :: CacheRecord Effect String }

createTestState :: TestState
createTestState = 
  { testValue: create
    { fetch: pure $ Just "fetched value"
    , flush: \_ -> pure unit
    , default: "default value"
    }
  }
