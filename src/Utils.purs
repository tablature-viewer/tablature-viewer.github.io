module Utils where

import Prelude

import Data.List (List(..), (:))
import Data.Tuple (Tuple, fst, snd)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)

debug :: forall a. String -> a -> a
debug msg = snd $ unsafePerformEffect $ log msg
  where snd _ b = b

foreach :: forall a b s. List a -> s -> (a -> s -> Tuple b s) -> List b
foreach Nil _ _ = Nil
foreach (x : xs) state loop = fst next : (foreach xs (snd next) loop)
  where next = loop x state
