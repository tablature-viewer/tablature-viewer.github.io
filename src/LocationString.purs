module LocationString where

import Prelude (Unit)
import Effect (Effect)

-- TODO: we could probably just use Web.HTML.Location
foreign import getLocationString :: Effect String
foreign import getLocationBaseString :: Effect String
foreign import getFragmentString :: Effect String
foreign import setFragmentString :: String -> Effect Unit
foreign import getQueryString :: Effect String
foreign import setQueryString :: String -> Effect Unit
