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

initialize  :: forall m . MonadEffect m => MonadState State m => m Unit
initialize = do
  tablatureText <- Cache.read cachedTablatureText
  if tablatureText == ""
  then modifyState _ { mode = EditMode }
  else modifyState _ { mode = ViewMode }

toggleEditMode  :: forall m . MonadEffect m => MonadState State m => m Unit
toggleEditMode = do
  state <- getState
  case state.mode of
    EditMode -> do
      -- FIXNOW
      -- tablatureText <- lift getTablatureTextFromEditor
      -- Cache.write _tablatureText tablatureText
      modifyState _ { mode = ViewMode }
      -- lift focusTablatureContainer
    ViewMode -> do
      modifyState _ { mode = EditMode }
      tablatureText <- Cache.read cachedTablatureText
      pure unit
      -- lift $ setTablatureTextInEditor tablatureText
      -- Don't focus the textarea, as the cursor position will be put at the end (which also sometimes makes the window jump)

toggleTabNormalization  :: forall m . MonadEffect m => MonadState State m => m Unit
toggleTabNormalization = do
  tabNormalizationEnabled <- Cache.read cachedTabNormalizationEnabled
  Cache.write cachedTabNormalizationEnabled (not tabNormalizationEnabled)

toggleTabDozenalization  :: forall m . MonadEffect m => MonadState State m => m Unit
toggleTabDozenalization = do
  ignoreDozenalization <- Cache.read cachedIgnoreDozenalization
  if ignoreDozenalization
  then pure unit
  else do
    tabDozenalizationEnabled <- Cache.read cachedTabDozenalizationEnabled
    Cache.write cachedTabDozenalizationEnabled (not tabDozenalizationEnabled)

toggleChordDozenalization  :: forall m . MonadEffect m => MonadState State m => m Unit
toggleChordDozenalization = do
  ignoreDozenalization <- Cache.read cachedIgnoreDozenalization
  if ignoreDozenalization
  then pure unit
  else do
    chordDozenalizationEnabled <- Cache.read cachedChordDozenalizationEnabled
    Cache.write cachedChordDozenalizationEnabled (not chordDozenalizationEnabled)

copyShortUrl  :: forall m . MonadAff m => MonadState State m => m Unit
copyShortUrl = do
  longUrl <- liftEffect getLocationString
  maybeShortUrl <- liftAff $ createShortUrl longUrl
  liftEffect $ case maybeShortUrl of
    Just shortUrl -> copyToClipboard shortUrl
    Nothing -> pure unit

increaseTransposition  :: forall m . MonadEffect m => MonadState State m => m Unit
increaseTransposition = do
  transposition <- Cache.read cachedTransposition
  Cache.write cachedTransposition $ succTransposition transposition

decreaseTransposition  :: forall m . MonadEffect m => MonadState State m => m Unit
decreaseTransposition = do
  transposition <- Cache.read cachedTransposition
  Cache.write cachedTransposition $ predTransposition transposition
