module AppActions where

import Prelude

import AppState (ActiveMenu(..), Mode(..), State, Url, _activeMenu, _autoscrollSpeed, _mode, _searchPhrase, _urlParams, chordDozenalizationEnabledCache, chordNormalizationEnabledCache, ignoreDozenalizationCache, setState, tabDozenalizationEnabledCache, tabNormalizationEnabledCache, tablatureTextCache, urlParamsCache, viewState)
import AppUrl (resetUrlParams)
import Cache as Cache
import Clipboard (copyToClipboard)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.State (class MonadState)
import CuttlyUrlShortener (createShortUrl)
import Data.Enum (pred, succ)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import LocationString (getLocationString)
import TablatureDocument (predTransposition, succTransposition)
import TablatureRewriter (NoteOrientation)
import UGScraper (execSearch, fetchTabFromUrl)

data Action
  = Initialize
  | ClickFileMenu
  | ClickSettingsMenu
  | ClickNoMenu
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

initialize :: forall m. MonadAff m => MonadState State m => m Unit
initialize = do
  tablatureText <- Cache.read tablatureTextCache
  if tablatureText == "" then setState _mode EditMode
  else setState _mode ViewMode

clickNoMenu :: forall m. MonadAff m => MonadState State m => m Unit
clickNoMenu = setState _activeMenu NoMenu

clickSettingsMenu :: forall m. MonadAff m => MonadState State m => m Unit
clickSettingsMenu = do
  activeMenu <- viewState _activeMenu
  if activeMenu == SettingsMenu then setState _activeMenu NoMenu
  else setState _activeMenu SettingsMenu

clickFileMenu :: forall m. MonadAff m => MonadState State m => m Unit
clickFileMenu = do
  activeMenu <- viewState _activeMenu
  if activeMenu == FileMenu then setState _activeMenu NoMenu
  else setState _activeMenu FileMenu

-- TODO: store scrollspeed somewhere external?
increaseAutoscrollSpeed :: forall m. MonadAff m => MonadState State m => m Unit
increaseAutoscrollSpeed = do
  currentSpeed <- viewState _autoscrollSpeed
  case succ currentSpeed of
    Nothing -> pure unit
    Just speed -> setState _autoscrollSpeed speed

decreaseAutoscrollSpeed :: forall m. MonadAff m => MonadState State m => m Unit
decreaseAutoscrollSpeed = do
  currentSpeed <- viewState _autoscrollSpeed
  case pred currentSpeed of
    Nothing -> pure unit
    Just speed -> setState _autoscrollSpeed speed

toggleTabNormalization :: forall m. MonadAff m => MonadState State m => m Unit
toggleTabNormalization = do
  tabNormalizationEnabled <- Cache.read tabNormalizationEnabledCache
  Cache.write tabNormalizationEnabledCache (not tabNormalizationEnabled)

toggleTabDozenalization :: forall m. MonadAff m => MonadState State m => m Unit
toggleTabDozenalization = do
  ignoreDozenalization <- Cache.read ignoreDozenalizationCache
  if ignoreDozenalization then pure unit
  else do
    tabDozenalizationEnabled <- Cache.read tabDozenalizationEnabledCache
    Cache.write tabDozenalizationEnabledCache (not tabDozenalizationEnabled)

toggleChordNormalization :: forall m. MonadAff m => MonadState State m => m Unit
toggleChordNormalization = do
  chordNormalizationEnabled <- Cache.read chordNormalizationEnabledCache
  Cache.write chordNormalizationEnabledCache (not chordNormalizationEnabled)

toggleChordDozenalization :: forall m. MonadAff m => MonadState State m => m Unit
toggleChordDozenalization = do
  ignoreDozenalization <- Cache.read ignoreDozenalizationCache
  if ignoreDozenalization then pure unit
  else do
    chordDozenalizationEnabled <- Cache.read chordDozenalizationEnabledCache
    Cache.write chordDozenalizationEnabledCache (not chordDozenalizationEnabled)

createAndCopyShortUrl :: forall m. MonadAff m => MonadState State m => m Unit
createAndCopyShortUrl = do
  longUrl <- liftEffect getLocationString
  maybeShortUrl <- liftAff $ runMaybeT (createShortUrl longUrl)
  liftEffect $ case maybeShortUrl of
    Just shortUrl -> copyToClipboard shortUrl
    Nothing -> pure unit

increaseTransposition :: forall m. MonadAff m => MonadState State m => m Unit
increaseTransposition = do
  urlParams <- Cache.read urlParamsCache
  Cache.write urlParamsCache $ urlParams { transposition = succTransposition urlParams.transposition }

decreaseTransposition :: forall m. MonadAff m => MonadState State m => m Unit
decreaseTransposition = do
  urlParams <- Cache.read urlParamsCache
  Cache.write urlParamsCache $ urlParams { transposition = predTransposition urlParams.transposition }

setNoteOrientation :: forall m. MonadAff m => NoteOrientation -> MonadState State m => m Unit
setNoteOrientation noteOrientation = do
  urlParams <- Cache.read urlParamsCache
  Cache.write urlParamsCache $ urlParams { noteOrientation = noteOrientation }

toggleSearch :: forall m. MonadAff m => MonadState State m => m Unit
toggleSearch = do
  currentMode <- viewState _mode
  tablatureText <- Cache.read tablatureTextCache
  setState _activeMenu NoMenu
  setState _mode
    if currentMode == SearchMode then
      if tablatureText == "" then EditMode
      else ViewMode
    else SearchMode

searchInput :: forall m. MonadAff m => MonadState State m => String -> m Unit
searchInput phrase = do
  setState _searchPhrase (Just phrase)
  execSearch phrase

importFromUrl :: forall m. MonadAff m => MonadState State m => Url -> m Unit
importFromUrl url = map (const unit) $ runMaybeT do
  tablatureText <- fetchTabFromUrl url
  Cache.write tablatureTextCache tablatureText
  liftEffect resetUrlParams
  Cache.invalidate _urlParams
  setState _mode ViewMode
