module Utils where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.State (class MonadState)
import Data.Array as Array
import Data.Enum (class Enum)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.Tuple (Tuple, fst, snd)

-- Show is for debugging, Print has to give a string that is actually how it is supposed to be presented to the user.
class Print a where
  print :: a -> String

class Enum a <= CyclicEnum a where
  succ' :: a -> a
  pred' :: a -> a

foreach :: forall a b s. s -> List a -> (s -> a -> Tuple s b) -> List b
foreach _ Nil _ = Nil
foreach state (x : xs) loop = snd next : (foreach (fst next) xs loop)
  where
  next = loop state x

foreach' :: forall a s. s -> List a -> (s -> a -> s) -> s
foreach' state Nil _ = state
foreach' state (x : xs) loop = foreach' nextState xs loop
  where
  nextState = loop state x

foreachM :: forall m a s. MonadState s m => List a -> (a -> m Unit) -> m Unit
foreachM Nil _ = pure unit
foreachM (x : xs) loop = do
  loop x
  foreachM xs loop

-- traverse in the base monad, then filter the results
mapMaybeM :: forall m a b. Monad m => (a -> m (Maybe b)) -> Array a -> m (Array b)
mapMaybeM f array = do
  maybes <- traverse f array
  values <- pure $ Array.mapMaybe identity maybes
  pure values

-- traverse in the base monad, then filter the results
mapMaybeT :: forall m a b. Monad m => (a -> MaybeT m b) -> Array a -> m (Array b)
mapMaybeT f array = do
  maybes <- traverse (runMaybeT <<< f) array
  values <- pure $ Array.mapMaybe identity maybes
  pure values

applyUntilIdempotent :: forall a. (Eq a) => (a -> a) -> a -> a
applyUntilIdempotent f x = if result == x then result else applyUntilIdempotent f result
  where
  result = f x

unsafeTestRegex :: String -> String -> Boolean
unsafeTestRegex patternString text = test (unsafeRegex patternString noFlags) text
