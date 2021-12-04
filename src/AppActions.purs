module AppActions where

import AppState
import Prelude

import Cache as Cache
import Clipboard (copyToClipboard)
import Control.Monad.State (class MonadState)
import Data.Enum (pred, succ)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import LocationString (getLocationString)
import TablatureDocument (predTransposition, succTransposition)
import UrlShortener (createShortUrl)

data Action
  = Initialize 
  | ToggleEditMode 
  | ToggleTabNormalization 
  | ToggleTabDozenalization 
  | ToggleChordDozenalization 
  | CopyShortUrl
  | ToggleAutoscroll
  | IncreaseAutoscrollSpeed
  | DecreaseAutoscrollSpeed
  | IncreaseTransposition
  | DecreaseTransposition


-- TODO: store scrollspeed somewhere
increaseAutoscrollSpeed :: forall m . MonadEffect m => MonadState State m => m Unit
increaseAutoscrollSpeed = do
  state <- getState
  case succ state.autoscrollSpeed of
    Nothing -> pure unit
    Just speed -> modifyState _ { autoscrollSpeed = speed }
  modifyState _ { autoscroll = true }

decreaseAutoscrollSpeed :: forall m . MonadEffect m => MonadState State m => m Unit
decreaseAutoscrollSpeed = do
  state <- getState
  case pred state.autoscrollSpeed of
    Nothing -> pure unit
    Just speed -> modifyState _ { autoscrollSpeed = speed }
  modifyState _ { autoscroll = true }

initialize :: forall m . MonadEffect m => MonadState State m => m Unit
initialize = do
  tablatureText <- Cache.read tablatureTextCache
  if tablatureText == ""
  then modifyState _ { mode = EditMode }
  else modifyState _ { mode = ViewMode }

toggleTabNormalization :: forall m . MonadEffect m => MonadState State m => m Unit
toggleTabNormalization = do
  tabNormalizationEnabled <- Cache.read tabNormalizationEnabledCache
  Cache.write tabNormalizationEnabledCache (not tabNormalizationEnabled)

toggleTabDozenalization :: forall m . MonadEffect m => MonadState State m => m Unit
toggleTabDozenalization = do
  ignoreDozenalization <- Cache.read ignoreDozenalizationCache
  if ignoreDozenalization
  then pure unit
  else do
    tabDozenalizationEnabled <- Cache.read tabDozenalizationEnabledCache
    Cache.write tabDozenalizationEnabledCache (not tabDozenalizationEnabled)

toggleChordDozenalization :: forall m . MonadEffect m => MonadState State m => m Unit
toggleChordDozenalization = do
  ignoreDozenalization <- Cache.read ignoreDozenalizationCache
  if ignoreDozenalization
  then pure unit
  else do
    chordDozenalizationEnabled <- Cache.read chordDozenalizationEnabledCache
    Cache.write chordDozenalizationEnabledCache (not chordDozenalizationEnabled)

copyShortUrl :: forall m . MonadAff m => MonadState State m => m Unit
copyShortUrl = do
  longUrl <- liftEffect getLocationString
  maybeShortUrl <- liftAff $ createShortUrl longUrl
  liftEffect $ case maybeShortUrl of
    Just shortUrl -> copyToClipboard shortUrl
    Nothing -> pure unit

increaseTransposition :: forall m . MonadEffect m => MonadState State m => m Unit
increaseTransposition = do
  transposition <- Cache.read transpositionCache
  Cache.write transpositionCache $ succTransposition transposition

decreaseTransposition :: forall m . MonadEffect m => MonadState State m => m Unit
decreaseTransposition = do
  transposition <- Cache.read transpositionCache
  Cache.write transpositionCache $ predTransposition transposition
