module AppActions where

import AppState
import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Cache as Cache
import Clipboard (copyToClipboard)
import Control.Monad.State (class MonadState)
import Data.Argonaut.Core (Json, caseJsonObject, caseJsonString)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), hush)
import Data.Enum (pred, succ)
import Data.Maybe (Maybe(..), maybe)
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Foreign.Object (lookup)
import LocationString (getLocationString)
import TablatureDocument (predTransposition, succTransposition)
import TablatureRewriter (NoteOrientation)
import UrlShortener (createShortUrl)
import Web.DOM.DOMParser (makeDOMParser, parseHTMLFromString)
import Web.DOM.Document (toParentNode)
import Web.DOM.Element (getAttribute)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)

foreign import _askUrl :: Effect String

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
  | ImportFromUrl

-- TODO: reduce the case statements
importFromUrl :: forall m. MonadAff m => MonadState State m => m Unit
importFromUrl = do
  url <- liftEffect _askUrl
  let corsUrl = "https://api.codetabs.com/v1/proxy?quest=" <> url
  response <- liftAff $ AX.get ResponseFormat.string corsUrl
  maybeResult <- liftEffect case response of
    Left err -> do
      Console.error $ "GET UG response failed to decode: " <> AX.printError err
      pure Nothing
    Right result -> pure $ Just result
  maybeTablatureText <- liftEffect case maybeResult of
    Nothing -> pure Nothing
    Just result -> extractTab result.body
  case maybeTablatureText of
    Nothing -> pure unit
    Just tablatureText -> Cache.write tablatureTextCache tablatureText

extractTab :: String -> Effect (Maybe String)
extractTab input = do
  parser <- makeDOMParser
  eitherDoc <- parseHTMLFromString input parser
  maybeElem <- case hush eitherDoc of
    Nothing -> pure Nothing
    Just doc -> querySelector (QuerySelector ".js-store") (toParentNode doc)
  maybeJsonString <- case maybeElem of
    Nothing -> pure Nothing
    Just elem -> getAttribute "data-content" elem
  maybeJson <- case maybeJsonString of
    Nothing -> pure Nothing
    Just jsonString -> pure $ hush $ jsonParser jsonString
  maybeRawTab <- pure case maybeJson of
    Nothing -> Nothing
    Just json -> json # child "store" >>= child "page" >>= child "data" >>= child "tab_view" >>= child "wiki_tab" >>= child "content" >>= string
  cleanTab <- pure case maybeRawTab of
    Nothing -> Nothing
    Just rawTab -> Just $ Regex.replace (unsafeRegex """\[\/?(ch|tab)\]""" global) "" rawTab
  pure cleanTab
  where
  child :: String -> Json -> Maybe Json
  child name json = caseJsonObject Nothing (\object -> lookup name object) json

  string :: Json -> Maybe String
  string json = caseJsonString Nothing (\s -> Just s) json

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
  if tablatureText == "" then setState _mode EditMode
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
