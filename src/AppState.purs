module AppState where

import Cache
import Prelude

import AppUrl (getTablatureTextFromUrl, getTranspositionFromUrl, saveTablatureToUrl, setAppQueryString)
import AutoscrollSpeed (AutoscrollSpeed(..))
import Control.Monad.State (class MonadState)
import Control.Monad.State as MonadState
import Data.Lens.Barlow (barlow, key)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String.Regex (test)
import Data.String.Regex.Flags (ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Timer (IntervalId)
import LocalStorage (getLocalStorageBoolean, setLocalStorageBoolean)
import TablatureDocument (RenderingOptions, TablatureDocument, Transposition(..), getTitle)
import TablatureParser (tryParseTablature)
import TablatureRewriter (rewriteTablatureDocument)

data Mode = ViewMode | EditMode

newtype State = State StateRecord
type StateRecord =
  { mode :: Mode
  , autoscrollTimer :: Maybe IntervalId
  , autoscrollSpeed :: AutoscrollSpeed
  , autoscroll :: Boolean
  -- Store the scrollTop in the state before actions so we can restore the expected scrollTop when switching views
  , scrollTop :: Number
  , loading :: Boolean
  , tablatureText :: CacheEntry State String
  , parseResult :: CacheEntry State TablatureDocument
  , rewriteResult :: CacheEntry State TablatureDocument
  , tablatureTitle :: CacheEntry State String
  , tabNormalizationEnabled :: CacheEntry State Boolean
  , tabDozenalizationEnabled :: CacheEntry State Boolean
  , chordDozenalizationEnabled :: CacheEntry State Boolean
  -- For tabs that are already dozenal themselves we want to ignore any dozenalization settings
  , ignoreDozenalization :: CacheEntry State Boolean
  , transposition :: CacheEntry State Transposition
  }
derive instance Newtype State _

initialState :: forall input . input -> State
initialState _ = State
  { mode: EditMode
  , loading: false
  , scrollTop: 0.0
  , autoscroll: false
  , autoscrollTimer: Nothing
  , autoscrollSpeed: Normal
  , tablatureText: buildCache ""
  , parseResult: buildCache Nil
  , rewriteResult: buildCache Nil
  , tablatureTitle: buildCache "Tab viewer"
  , tabNormalizationEnabled: buildCache true
  , tabDozenalizationEnabled: buildCache false
  , chordDozenalizationEnabled: buildCache false
  , ignoreDozenalization: buildCache false
  , transposition: buildCache (Transposition 0)
  }

modifyState :: forall m . MonadState State m => (StateRecord -> StateRecord) -> m Unit
modifyState f = MonadState.modify_ \(State s) -> State (f s)

getState :: forall m . MonadState State m => m StateRecord
getState = do
  State state <- MonadState.get
  pure state


type AppStateReadWriteCacheUnit a = forall m . MonadEffect m => MonadState State m => ReadWriteCacheUnit State a () m
type AppStateReadCacheUnit a = forall m . MonadEffect m => MonadState State m => ReadableCacheUnit State a () m

transpositionCache :: AppStateReadWriteCacheUnit Transposition
transpositionCache =
  { entry: _transposition
  , flush: Flush $ \value -> liftEffect $ setAppQueryString { transposition: value }
  , fetch: Fetch $ liftEffect getTranspositionFromUrl
  }
_transposition :: EntryKey State Transposition
_transposition = EntryKey (barlow (key :: _ "!.transposition"))

tablatureTextCache :: AppStateReadWriteCacheUnit String
tablatureTextCache =
  { entry: _tablatureText
  , flush: Flush $ \value -> liftEffect $ saveTablatureToUrl value
  , fetch: Fetch $ liftEffect $ getTablatureTextFromUrl <#> Just
  }
_tablatureText :: EntryKey State String
_tablatureText = EntryKey (barlow (key :: _ "!.tablatureText"))

parseResultCache :: AppStateReadCacheUnit TablatureDocument
parseResultCache =
  { entry: entryKey
  , fetch: Fetch $ do
      tablatureText <- depend entryKey tablatureTextCache
      pure $ tryParseTablature tablatureText
  } where entryKey = _parseResult 
_parseResult :: EntryKey State TablatureDocument
_parseResult = EntryKey (barlow (key :: _ "!.parseResult"))

rewriteResultCache :: AppStateReadCacheUnit TablatureDocument
rewriteResultCache =
  { entry: entryKey
  , fetch: Fetch $ do
      parseResult <- depend entryKey parseResultCache
      renderingOptions <- getRenderingOptions
      pure $ Just $ rewriteTablatureDocument renderingOptions parseResult
  } where entryKey = _rewriteResult 
_rewriteResult :: EntryKey State TablatureDocument
_rewriteResult = EntryKey (barlow (key :: _ "!.rewriteResult"))

tablatureTitleCache :: AppStateReadCacheUnit String
tablatureTitleCache =
  { entry: entryKey
  , fetch: Fetch $ do
      parseResult <- depend entryKey parseResultCache
      pure $ getTitle parseResult
  } where entryKey = _tablatureTitle 
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

ignoreDozenalizationCache :: AppStateReadCacheUnit Boolean
ignoreDozenalizationCache =
  { entry: entryKey
  , fetch: Fetch $ do
      tablatureTitle <- depend entryKey tablatureTitleCache
      pure $ Just $ test (unsafeRegex "dozenal" ignoreCase) tablatureTitle
  } where entryKey = _ignoreDozenalization
_ignoreDozenalization :: EntryKey State Boolean
_ignoreDozenalization = EntryKey (barlow (key :: _ "!.ignoreDozenalization"))

-- TODO: move renderingoptions also to cache?
getRenderingOptions :: forall m . MonadState State m => MonadEffect m => m RenderingOptions
getRenderingOptions = do
  tabNormalizationEnabled <- read tabNormalizationEnabledCache
  tabDozenalizationEnabled <- read tabDozenalizationEnabledCache
  chordDozenalizationEnabled <- read chordDozenalizationEnabledCache
  ignoreDozenalization <- read ignoreDozenalizationCache
  transposition <- read transpositionCache
  pure $ { dozenalizeTabs: tabDozenalizationEnabled && not ignoreDozenalization
  , dozenalizeChords: chordDozenalizationEnabled && not ignoreDozenalization
  , normalizeTabs: tabNormalizationEnabled
  , transposition: transposition }
