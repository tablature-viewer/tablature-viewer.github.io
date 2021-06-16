module Clipboard where

import Prelude
import Effect (Effect)

-- TODO: we can probably just call window.prompt in purescript
foreign import copyToClipboard :: String -> Effect Unit
