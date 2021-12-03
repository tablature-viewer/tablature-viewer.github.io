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
import Effect.Class.Console as Console

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

testFetch :: StateT TestState Effect (Maybe Boolean)
testFetch = do
  value2 <- depend _testValue2 _testValue1
  pure $ Just true

testFlush :: forall m a . Monad m => a -> m Unit
testFlush value = pure unit

type TestCache = ReadWriteCacheUnit TestState Boolean () (StateT TestState Effect)
testCache :: TestCache
testCache = readWriteCache
  { default: false
  , fetch: Fetch testFetch
  , flush: Flush testFlush
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
