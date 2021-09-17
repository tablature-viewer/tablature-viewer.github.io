module Utils where

import Prelude

import Effect.Console (error, log)
import Effect.Unsafe (unsafePerformEffect)

unsafeError :: String -> Unit
unsafeError msg = unsafePerformEffect $ error msg

unsafeLog :: String -> Unit
unsafeLog msg = unsafePerformEffect $ log msg

unsafeLogThen :: forall a. String -> a -> a
unsafeLogThen msg = snd (unsafePerformEffect $ log msg)

snd :: forall a b. a -> b -> b
snd _ b = b
