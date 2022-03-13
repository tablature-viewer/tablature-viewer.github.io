module AppActions where

import AppState
import Prelude

import Cache as Cache
import Clipboard (copyToClipboard)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.State (class MonadState)
import Data.Enum (pred, succ)
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import LocationString (getLocationString)
import TablatureDocument (predTransposition, succTransposition)
import TablatureRewriter (NoteOrientation)
import UGScraper (execSearch, fetchTabFromUrl)
import UrlShortener (createShortUrl)

data Action
  = Initialize
  | ToggleEditMode
  | ToggleTabNormalization
  | ToggleTabDozenalization
  | ToggleChordNormalization
  | ToggleChordDozenalization
  | CreateShortUrl
  | ToggleAutoscroll
  | IncreaseAutoscrollSpeed
  | DecreaseAutoscrollSpeed
  | IncreaseTransposition
  | DecreaseTransposition
  | FlatNoteOrientation
  | SharpNoteOrientation
  | DefaultNoteOrientation
  | ToggleSearch
  | SearchInput String
  | ImportFromUrl Url

-- TODO: store scrollspeed somewhere external?
increaseAutoscrollSpeed :: forall m. MonadEffect m => MonadState State m => m Unit
increaseAutoscrollSpeed = do
  currentSpeed <- viewState _autoscrollSpeed
  case succ currentSpeed of
    Nothing -> pure unit
    Just speed -> setState _autoscrollSpeed speed

decreaseAutoscrollSpeed :: forall m. MonadEffect m => MonadState State m => m Unit
decreaseAutoscrollSpeed = do
  currentSpeed <- viewState _autoscrollSpeed
  case pred currentSpeed of
    Nothing -> pure unit
    Just speed -> setState _autoscrollSpeed speed

initialize :: forall m. MonadEffect m => MonadState State m => m Unit
initialize = do
  tablatureText <- Cache.read tablatureTextCache
  if tablatureText == "" then setState _mode SearchMode
  else setState _mode ViewMode

toggleTabNormalization :: forall m. MonadEffect m => MonadState State m => m Unit
toggleTabNormalization = do
  tabNormalizationEnabled <- Cache.read tabNormalizationEnabledCache
  Cache.write tabNormalizationEnabledCache (not tabNormalizationEnabled)

toggleTabDozenalization :: forall m. MonadEffect m => MonadState State m => m Unit
toggleTabDozenalization = do
  ignoreDozenalization <- Cache.read ignoreDozenalizationCache
  if ignoreDozenalization then pure unit
  else do
    tabDozenalizationEnabled <- Cache.read tabDozenalizationEnabledCache
    Cache.write tabDozenalizationEnabledCache (not tabDozenalizationEnabled)

toggleChordNormalization :: forall m. MonadEffect m => MonadState State m => m Unit
toggleChordNormalization = do
  chordNormalizationEnabled <- Cache.read chordNormalizationEnabledCache
  Cache.write chordNormalizationEnabledCache (not chordNormalizationEnabled)

toggleChordDozenalization :: forall m. MonadEffect m => MonadState State m => m Unit
toggleChordDozenalization = do
  ignoreDozenalization <- Cache.read ignoreDozenalizationCache
  if ignoreDozenalization then pure unit
  else do
    chordDozenalizationEnabled <- Cache.read chordDozenalizationEnabledCache
    Cache.write chordDozenalizationEnabledCache (not chordDozenalizationEnabled)

createAndCopyShortUrl :: forall m. MonadAff m => MonadState State m => m Unit
createAndCopyShortUrl = do
  longUrl <- liftEffect getLocationString
  maybeShortUrl <- liftAff $ createShortUrl longUrl
  liftEffect $ case maybeShortUrl of
    Just shortUrl -> copyToClipboard shortUrl
    Nothing -> pure unit

increaseTransposition :: forall m. MonadEffect m => MonadState State m => m Unit
increaseTransposition = do
  urlParams <- Cache.read urlParamsCache
  Cache.write urlParamsCache $ urlParams { transposition = succTransposition urlParams.transposition }

decreaseTransposition :: forall m. MonadEffect m => MonadState State m => m Unit
decreaseTransposition = do
  urlParams <- Cache.read urlParamsCache
  Cache.write urlParamsCache $ urlParams { transposition = predTransposition urlParams.transposition }

setNoteOrientation :: forall m. MonadEffect m => NoteOrientation -> MonadState State m => m Unit
setNoteOrientation noteOrientation = do
  urlParams <- Cache.read urlParamsCache
  Cache.write urlParamsCache $ urlParams { noteOrientation = noteOrientation }

toggleSearch :: forall m. MonadAff m => MonadState State m => m Unit
toggleSearch = do
  currentMode <- viewState _mode
  setState _mode if currentMode == SearchMode then ViewMode else SearchMode

searchInput :: forall m. MonadAff m => MonadState State m => String -> m Unit
searchInput phrase = do
  setState _searchPhrase (Just phrase)
  execSearch phrase

importFromUrl :: forall m. MonadAff m => MonadState State m => Url -> m Unit
importFromUrl url = map (const unit) $ runMaybeT do
  tablatureText <- fetchTabFromUrl url
  Cache.write tablatureTextCache tablatureText
  setState _mode ViewMode
