module Clipboard where

import Prelude
import Effect (Effect)

-- TODO: we can probably just call window.prompt in purescript
-- TODO: there exists some form of standardization (navigator.clipboard)
foreign import copyToClipboard :: String -> Effect Unit
