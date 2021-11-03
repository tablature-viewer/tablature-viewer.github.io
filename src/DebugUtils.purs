module DebugUtils where

import Prelude

import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)

debug :: forall a. String -> a -> a
debug msg = snd $ unsafePerformEffect $ log msg
  where snd _ b = b

debugVal_ :: forall a. Show a => a -> a
debugVal_ value = snd (unsafePerformEffect $ log $ show value) value
  where snd _ b = b

debugVal :: forall a. Show a => String -> a -> a
debugVal msg value = snd (unsafePerformEffect $ log $ msg <> ": " <> show value) value
  where snd _ b = b

debugM :: forall m. Monad m => String -> m Unit
debugM msg = pure $ unsafePerformEffect $ log msg
