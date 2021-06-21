module LocalStorage where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

foreign import setLocalStorage :: String -> String -> Effect Unit
foreign import getLocalStorage :: String -> Effect String

getLocalStorageBoolean :: String -> Effect (Maybe Boolean)
getLocalStorageBoolean key = do
  result <- getLocalStorage key
  pure $ case result of
    "true" -> Just true
    "false" -> Just false
    _ -> Nothing
