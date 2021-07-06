module LocationString where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Foreign (Foreign, readString)

-- TODO: we could probably just use Web.HTML.Location
foreign import getLocationString :: Effect String
foreign import getLocationBaseString :: Effect String
foreign import setLocationString :: String -> Effect Unit
foreign import getFragmentString :: Effect String
foreign import setFragmentString :: String -> Effect Unit
foreign import getQueryString :: Effect String
foreign import setQueryString :: String -> Effect Unit

foreign import _getQueryParam :: String -> Effect Foreign

getQueryParam :: String -> Effect (Maybe String)
getQueryParam paramName = do
  result <- _getQueryParam paramName
  pure $ case result # readString # runExcept of
    Right s -> Just s
    _ -> Nothing
