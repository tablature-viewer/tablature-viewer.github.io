module Utils where

import Prelude

import Data.List (List(..), (:))
import Data.Tuple (Tuple, fst, snd)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)

debug :: forall a. String -> a -> a
debug msg = snd $ unsafePerformEffect $ log msg
  where snd _ b = b

foreach :: forall a b s. s -> List a -> (s -> a -> Tuple s b) -> List b
foreach _ Nil _ = Nil
foreach state (x : xs) loop = snd next : (foreach (fst next) xs loop)
  where next = loop state x
