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

setLocalStorageBoolean :: String -> Boolean -> Effect Unit
setLocalStorageBoolean key value = setLocalStorage key (show value)

getLocalStorageBooleanWithDefault :: String -> Boolean -> Effect Boolean
getLocalStorageBooleanWithDefault key default = do
  result <- getLocalStorageBoolean key
  case result of
    Nothing -> pure default
    Just bool -> pure bool
