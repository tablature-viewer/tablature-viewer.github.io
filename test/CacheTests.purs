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
  state <- pure {}
  _ <- execStateT run state
  pure unit

type State = {}

run :: StateT State Effect Unit
run = do
  pure unit
