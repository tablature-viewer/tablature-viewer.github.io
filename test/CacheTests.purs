module CacheTests where

import Cache
import Prelude

import Control.Monad.State (class MonadState, StateT(..), execStateT)
import Control.Monad.State as MonadState
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Barlow (barlow, key)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)

main :: Effect Unit
main = do
  state <- pure createTestState
  _ <- execStateT run state
  pure unit

run :: StateT TestState Effect Unit
run = do
  pure unit
  value <- read _testValue1
  write _testValue1 value
  purge _testValue1
  pure unit

testFetch :: forall m . MonadState TestState m => m (Maybe Boolean)
testFetch = do
  value2 <- depend _testValue2 _testValue1
  pure $ Just true

testFlush :: forall m a . Monad m => a -> m Unit
testFlush value = pure unit

type TestCache = CacheUnit TestState Boolean
testCache :: TestCache
testCache = 
  { value: NoValue
  , fetch: Fetch testFetch
  , flush: Flush testFlush
  , default: false
  , dependants: Nil
  }

newtype TestState = TestState
  { testValue1 :: TestCache
  , testValue2 :: TestCache }
-- data TestState = TestState TestCache TestCache
derive instance Newtype TestState _
-- derive instance Generic TestState _

-- _testValue1 :: CacheKey TestState Boolean
_testValue1 = barlow (key :: _ "!.testValue1")
_testValue2 :: Lens' TestState TestCache
_testValue2 = barlow (key :: _ "!.testValue2")

createTestState :: TestState
createTestState = TestState
  { testValue1: testCache
  , testValue2: testCache }
-- createTestState = TestState testCache testCache


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
