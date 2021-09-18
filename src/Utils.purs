module Utils where

import Prelude

import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)

debug :: forall a. String -> a -> a
debug msg = snd $ unsafePerformEffect $ log msg

snd :: forall a b. a -> b -> b
snd _ b = b
