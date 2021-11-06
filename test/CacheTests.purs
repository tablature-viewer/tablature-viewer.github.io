module CacheTests where

import Cache (class CacheDefault, class CacheEntry, class Dependable, class Purgeable, packPurgeableLens)
import Prelude

import Data.Lens (Lens')
import Data.Lens.Barlow (barlow, key)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)

main :: Effect Unit
main = do
  pure unit
  _ <- pure createTestState
  pure unit

newtype MyCachedValue = MyCachedValue (Maybe String)

newtype TestState = TestState
  { testValue1 :: MyCachedValue
  , testValue2 :: MyCachedValue }
derive instance Newtype TestState _

_testValue1 :: Lens' TestState MyCachedValue
_testValue1 = barlow (key :: _ "!.testValue1")
_testValue2 :: Lens' TestState MyCachedValue
_testValue2 = barlow (key :: _ "!.testValue2")

createTestState :: TestState
createTestState = TestState { testValue1: MyCachedValue Nothing, testValue2: MyCachedValue Nothing }

instance CacheEntry String MyCachedValue where
  getCacheValue (MyCachedValue c) = c
  setCacheValue (MyCachedValue c) newValue = MyCachedValue (Just newValue)
instance CacheDefault String MyCachedValue where
  default _ = ""
instance Purgeable MyCachedValue where
  purgeCacheValue _ = MyCachedValue Nothing
  hasCacheValue (MyCachedValue Nothing) = false
  hasCacheValue _ = true
instance Dependable TestState MyCachedValue where
  dependants _ = Cons (packPurgeableLens _testValue1) Nil


newtype Showable = Showable (forall r. (forall a. Show a => a -> r) -> r)

mkShowable :: forall a. Show a => a -> Showable
mkShowable a = Showable \k -> k a

unShowable :: forall r. (forall a. Show a => a -> r) -> Showable -> r
unShowable k1 (Showable k2) = k2 k1

test1 :: Showable
test1 = mkShowable 42
test2 :: String
-- test2 = unShowable (\a -> show a) test1
test2 = unShowable show test1
