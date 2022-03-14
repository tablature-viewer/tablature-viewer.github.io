module AppState where

import Cache
import Prelude

import AppUrl (UrlParams, getAppUrlParams, getTablatureTextFromUrl, saveTablatureToUrl, setAppQueryString)
import AutoscrollSpeed (AutoscrollSpeed(..))
import Control.Monad.State (class MonadState)
import Control.Monad.State as MonadState
import Data.Lens (Lens', over, set, view)
import Data.Lens.Barlow (barlow, key)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String.Regex (test)
import Data.String.Regex.Flags (ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Timer (IntervalId)
import LocalStorage (getLocalStorageBoolean, setLocalStorageBoolean)
import TablatureDocument (TablatureDocument, Transposition(..), getTitle)
import TablatureParser (tryParseTablature)
import TablatureRewriter (NoteOrientation(..), rewriteTablatureDocument)

data Mode = ViewMode | EditMode | SearchMode

derive instance Eq Mode

type Url = String

type SearchResult =
  { url :: Url
  , name :: String
  , artist :: String
  , rating :: Maybe Number
  , contentType :: Maybe String
  , marketingType :: Maybe String
  }

newtype State = State StateRecord

type StateRecord =
  { mode :: Mode
  , loading :: Boolean
  , scrollTop :: Number
  , searchPhrase :: Maybe String
  , searchResults :: Maybe (Array SearchResult)
  , autoscroll :: Boolean
  , autoscrollSpeed :: AutoscrollSpeed
  , autoscrollTimer :: Maybe IntervalId
  -- Store the scrollTop in the state before actions so we can restore the expected scrollTop when switching views
  , tablatureText :: CacheEntry State String
  , parseResult :: CacheEntry State TablatureDocument
  , rewriteResult :: CacheEntry State TablatureDocument
  , tablatureTitle :: CacheEntry State String
  , tabNormalizationEnabled :: CacheEntry State Boolean
  , tabDozenalizationEnabled :: CacheEntry State Boolean
  , chordNormalizationEnabled :: CacheEntry State Boolean
  , chordDozenalizationEnabled :: CacheEntry State Boolean
  , upperCaseNotes :: CacheEntry State Boolean
  -- For tabs that are already dozenal themselves we want to ignore any dozenalization settings
  , ignoreDozenalization :: CacheEntry State Boolean
  , urlParams :: CacheEntry State UrlParams
  }

derive instance Newtype State _

initialState :: forall input. input -> State
initialState _ = State
  { mode: EditMode
  , loading: false
  , scrollTop: 0.0
  , searchPhrase: Nothing
  , searchResults: Nothing
  , autoscroll: false
  , autoscrollTimer: Nothing
  , autoscrollSpeed: Normal
  , tablatureText: buildCache ""
  , parseResult: buildCache Nil
  , rewriteResult: buildCache Nil
  , tablatureTitle: buildCache "Tab viewer"
  , tabNormalizationEnabled: buildCache true
  , tabDozenalizationEnabled: buildCache false
  , chordNormalizationEnabled: buildCache false
  , chordDozenalizationEnabled: buildCache false
  , upperCaseNotes: buildCache true
  , ignoreDozenalization: buildCache false
  , urlParams: buildCache { transposition: Transposition 0, noteOrientation: Default }
  }

_mode :: Lens' State Mode
_mode = barlow (key :: _ "!.mode")

_loading :: Lens' State Boolean
_loading = barlow (key :: _ "!.loading")

_scrollTop :: Lens' State Number
_scrollTop = barlow (key :: _ "!.scrollTop")

_searchPhrase :: Lens' State (Maybe String)
_searchPhrase = barlow (key :: _ "!.searchPhrase")

_searchResults :: Lens' State (Maybe (Array SearchResult))
_searchResults = barlow (key :: _ "!.searchResults")

_autoscroll :: Lens' State Boolean
_autoscroll = barlow (key :: _ "!.autoscroll")

_autoscrollTimer :: Lens' State (Maybe IntervalId)
_autoscrollTimer = barlow (key :: _ "!.autoscrollTimer")

_autoscrollSpeed :: Lens' State AutoscrollSpeed
_autoscrollSpeed = barlow (key :: _ "!.autoscrollSpeed")

viewState :: forall a m. MonadState State m => Lens' State a -> m a
viewState _key = do
  state <- MonadState.get
  pure $ view _key state

setState :: forall a m. MonadState State m => Lens' State a -> a -> m Unit
setState _key value = do
  state <- MonadState.get
  MonadState.put $ set _key value state

overState :: forall a m. MonadState State m => Lens' State a -> (a -> a) -> m Unit
overState _key f = do
  state <- MonadState.get
  MonadState.put $ over _key f state

type AppStateReadWriteCacheUnit a = forall m. MonadAff m => MonadState State m => ReadWriteCacheUnit State a () m
type AppStateReadCacheUnit a = forall m. MonadAff m => MonadState State m => ReadableCacheUnit State a () m

urlParamsCache :: AppStateReadWriteCacheUnit UrlParams
urlParamsCache =
  { entry: _urlParams
  , flush: Flush \value -> liftEffect $ setAppQueryString value
  , fetch: Fetch $ liftEffect $ getAppUrlParams <#> Just
  }

_urlParams :: EntryKey State UrlParams
_urlParams = EntryKey (barlow (key :: _ "!.urlParams"))

tablatureTextCache :: AppStateReadWriteCacheUnit String
tablatureTextCache =
  { entry: _tablatureText
  , flush: Flush \value -> liftEffect $ saveTablatureToUrl value
  , fetch: Fetch $ do
      text <- liftEffect getTablatureTextFromUrl
      pure $ Just text
  }

_tablatureText :: EntryKey State String
_tablatureText = EntryKey (barlow (key :: _ "!.tablatureText"))

parseResultCache :: AppStateReadCacheUnit TablatureDocument
parseResultCache =
  { entry: entryKey
  , fetch: Fetch $ do
      tablatureText <- subscribe entryKey tablatureTextCache
      pure $ tryParseTablature tablatureText
  }
  where
  entryKey = _parseResult

_parseResult :: EntryKey State TablatureDocument
_parseResult = EntryKey (barlow (key :: _ "!.parseResult"))

rewriteResultCache :: AppStateReadCacheUnit TablatureDocument
rewriteResultCache =
  { entry: entryKey
  , fetch: Fetch $ do
      parseResult <- subscribe entryKey parseResultCache
      tabNormalizationEnabled <- subscribe entryKey tabNormalizationEnabledCache
      tabDozenalizationEnabled <- subscribe entryKey tabDozenalizationEnabledCache
      chordNormalizationEnabled <- subscribe entryKey chordNormalizationEnabledCache
      chordDozenalizationEnabled <- subscribe entryKey chordDozenalizationEnabledCache
      ignoreDozenalization <- subscribe entryKey ignoreDozenalizationCache
      urlParams <- subscribe entryKey urlParamsCache

      renderingOptions <- pure
        { dozenalizeTabs: tabDozenalizationEnabled && not ignoreDozenalization
        , dozenalizeChords: chordDozenalizationEnabled && not ignoreDozenalization
        , normalizeTabs: tabNormalizationEnabled
        , normalizeChords: chordNormalizationEnabled
        , transposition: urlParams.transposition
        , noteOrientation: urlParams.noteOrientation
        }
      pure $ Just $ rewriteTablatureDocument renderingOptions parseResult
  }
  where
  entryKey = _rewriteResult

_rewriteResult :: EntryKey State TablatureDocument
_rewriteResult = EntryKey (barlow (key :: _ "!.rewriteResult"))

tablatureTitleCache :: AppStateReadCacheUnit String
tablatureTitleCache =
  { entry: entryKey
  , fetch: Fetch $ do
      parseResult <- subscribe entryKey parseResultCache
      pure $ getTitle parseResult
  }
  where
  entryKey = _tablatureTitle

_tablatureTitle :: EntryKey State String
_tablatureTitle = EntryKey (barlow (key :: _ "!.tablatureTitle"))

localStorageBooleanCache :: String -> EntryKey State Boolean -> AppStateReadWriteCacheUnit Boolean
localStorageBooleanCache localStorageKey _key =
  { entry: _key
  , flush: Flush $ \value -> liftEffect $ setLocalStorageBoolean localStorageKey value
  , fetch: Fetch $ liftEffect $ getLocalStorageBoolean localStorageKey
  }

tabNormalizationEnabledCache :: AppStateReadWriteCacheUnit Boolean
tabNormalizationEnabledCache = localStorageBooleanCache "tabNormalizationEnabled" _tabNormalizationEnabled

_tabNormalizationEnabled :: EntryKey State Boolean
_tabNormalizationEnabled = EntryKey (barlow (key :: _ "!.tabNormalizationEnabled"))

tabDozenalizationEnabledCache :: AppStateReadWriteCacheUnit Boolean
tabDozenalizationEnabledCache = localStorageBooleanCache "tabDozenalizationEnabled" _tabDozenalizationEnabled

_tabDozenalizationEnabled :: EntryKey State Boolean
_tabDozenalizationEnabled = EntryKey (barlow (key :: _ "!.tabDozenalizationEnabled"))

chordDozenalizationEnabledCache :: AppStateReadWriteCacheUnit Boolean
chordDozenalizationEnabledCache = localStorageBooleanCache "chordDozenalizationEnabled" _chordDozenalizationEnabled

_chordDozenalizationEnabled :: EntryKey State Boolean
_chordDozenalizationEnabled = EntryKey (barlow (key :: _ "!.chordDozenalizationEnabled"))

chordNormalizationEnabledCache :: AppStateReadWriteCacheUnit Boolean
chordNormalizationEnabledCache = localStorageBooleanCache "chordNormalizationEnabled" _chordNormalizationEnabled

_chordNormalizationEnabled :: EntryKey State Boolean
_chordNormalizationEnabled = EntryKey (barlow (key :: _ "!.chordNormalizationEnabled"))

ignoreDozenalizationCache :: AppStateReadCacheUnit Boolean
ignoreDozenalizationCache =
  { entry: entryKey
  , fetch: Fetch $ do
      tablatureTitle <- subscribe entryKey tablatureTitleCache
      pure $ Just $ test (unsafeRegex "dozenal" ignoreCase) tablatureTitle
  }
  where
  entryKey = _ignoreDozenalization

_ignoreDozenalization :: EntryKey State Boolean
_ignoreDozenalization = EntryKey (barlow (key :: _ "!.ignoreDozenalization"))
