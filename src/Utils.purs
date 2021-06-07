module Utils where

import Effect.Console (error, log)
import Effect.Unsafe (unsafePerformEffect)
import Prelude (Unit, ($))

unsafeError :: String -> Unit
unsafeError msg = unsafePerformEffect $ error msg

unsafeLog :: String -> Unit
unsafeLog msg = unsafePerformEffect $ log msg
