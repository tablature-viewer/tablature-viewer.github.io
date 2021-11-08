module DebugUtils where

import Prelude

import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)

debug :: forall a. String -> a -> a
debug msg value = snd (unsafePerformEffect $ log $ msg) value
  where snd _ b = b

debug_ :: forall a. Show a => a -> a
debug_ value = snd (unsafePerformEffect $ log $ show value) value
  where snd _ b = b

debugM :: forall m s. Monad m => String -> Show s => s -> m Unit
debugM msg value = pure $ unsafePerformEffect $ log $ msg <> ": " <> show value

debugM_ :: forall m s. Monad m => Show s => s -> m Unit
debugM_ value = pure $ unsafePerformEffect $ log $ show value
