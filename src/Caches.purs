module Caches where

import Prelude
import Cache

import Control.Monad.State (class MonadState)
import Control.Monad.State as MonadState
import Data.Lens (Lens', over, view)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Type.Prelude (Proxy(..))
