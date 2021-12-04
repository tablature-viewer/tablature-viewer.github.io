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
  , tablatureText :: Entry State String
  , parseResult :: Entry State TablatureDocument
  , rewriteResult :: Entry State TablatureDocument
  , tablatureTitle :: Entry State String
  , tabNormalizationEnabled :: Entry State Boolean
  , tabDozenalizationEnabled :: Entry State Boolean
  , chordDozenalizationEnabled :: Entry State Boolean
  -- For tabs that are already dozenal themselves we want to ignore any dozenalization settings
  , ignoreDozenalization :: Entry State Boolean
  , transposition :: Entry State Transposition
  }
derive instance Newtype State _

modifyState :: forall m . MonadState State m => (StateRecord -> StateRecord) -> m Unit
modifyState f = MonadState.modify_ \(State s) -> State (f s)

getState :: forall m . MonadState State m => m StateRecord
getState = do
  State state <- MonadState.get
  pure state


type AppStateReadWriteCacheDef a m = ReadWriteCacheDef State a () m
type AppStateReadWriteCacheDef' a = forall m . MonadEffect m => MonadState State m => AppStateReadWriteCacheDef a m

type AppStateReadCacheDef a m = ReadableCacheDef State a () m
type AppStateReadCacheDef' a = forall m . MonadEffect m => MonadState State m => AppStateReadCacheDef a m

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

cachedTransposition :: AppStateReadWriteCacheDef' Transposition
cachedTransposition =
  { entry: _transposition
  , flush: Flush $ \value -> liftEffect $ setAppQueryString { transposition: value }
  , fetch: Fetch $ liftEffect getTranspositionFromUrl
  }
_transposition :: EntryKey State Transposition
_transposition = EntryKey (barlow (key :: _ "!.transposition"))

cachedTablatureText :: AppStateReadWriteCacheDef' String
cachedTablatureText =
  { entry: _tablatureText
  , flush: Flush $ \value -> liftEffect $ saveTablatureToUrl value
  , fetch: Fetch $ liftEffect $ getTablatureTextFromUrl <#> Just
  }
_tablatureText :: EntryKey State String
_tablatureText = EntryKey (barlow (key :: _ "!.tablatureText"))

cachedParseResult :: AppStateReadCacheDef' TablatureDocument
cachedParseResult =
  { entry: entryKey
  , fetch: Fetch $ do
      tablatureText <- depend entryKey cachedTablatureText
      pure $ tryParseTablature tablatureText
  } where entryKey = _parseResult 
_parseResult :: EntryKey State TablatureDocument
_parseResult = EntryKey (barlow (key :: _ "!.parseResult"))

cachedRewriteResult :: AppStateReadCacheDef' TablatureDocument
cachedRewriteResult =
  { entry: entryKey
  , fetch: Fetch $ do
      parseResult <- depend entryKey cachedParseResult
      renderingOptions <- getRenderingOptions
      pure $ Just $ rewriteTablatureDocument renderingOptions parseResult
  } where entryKey = _rewriteResult 
_rewriteResult :: EntryKey State TablatureDocument
_rewriteResult = EntryKey (barlow (key :: _ "!.rewriteResult"))

cachedTablatureTitle :: AppStateReadCacheDef' String
cachedTablatureTitle =
  { entry: entryKey
  , fetch: Fetch $ do
      parseResult <- depend entryKey cachedParseResult
      pure $ getTitle parseResult
  } where entryKey = _tablatureTitle 
_tablatureTitle :: EntryKey State String
_tablatureTitle = EntryKey (barlow (key :: _ "!.tablatureTitle"))

cachedLocalStorageBoolean :: String -> EntryKey State Boolean -> AppStateReadWriteCacheDef' Boolean
cachedLocalStorageBoolean localStorageKey _key =
  { entry: _key
  , flush: Flush $ \value -> liftEffect $ setLocalStorageBoolean localStorageKey value
  , fetch: Fetch $ liftEffect $ getLocalStorageBoolean localStorageKey
  }

cachedTabNormalizationEnabled :: AppStateReadWriteCacheDef' Boolean
cachedTabNormalizationEnabled = cachedLocalStorageBoolean "tabNormalizationEnabled" _tabNormalizationEnabled 
_tabNormalizationEnabled :: EntryKey State Boolean
_tabNormalizationEnabled = EntryKey (barlow (key :: _ "!.tabNormalizationEnabled"))

cachedTabDozenalizationEnabled :: AppStateReadWriteCacheDef' Boolean
cachedTabDozenalizationEnabled = cachedLocalStorageBoolean "tabDozenalizationEnabled" _tabDozenalizationEnabled 
_tabDozenalizationEnabled :: EntryKey State Boolean
_tabDozenalizationEnabled = EntryKey (barlow (key :: _ "!.tabDozenalizationEnabled"))

cachedChordDozenalizationEnabled :: AppStateReadWriteCacheDef' Boolean
cachedChordDozenalizationEnabled = cachedLocalStorageBoolean "chordDozenalizationEnabled" _chordDozenalizationEnabled 
_chordDozenalizationEnabled :: EntryKey State Boolean
_chordDozenalizationEnabled = EntryKey (barlow (key :: _ "!.chordDozenalizationEnabled"))

cachedIgnoreDozenalization :: AppStateReadCacheDef' Boolean
cachedIgnoreDozenalization =
  { entry: entryKey
  , fetch: Fetch $ do
      tablatureTitle <- depend entryKey cachedTablatureTitle
      pure $ Just $ test (unsafeRegex "dozenal" ignoreCase) tablatureTitle
  } where entryKey = _ignoreDozenalization
_ignoreDozenalization :: EntryKey State Boolean
_ignoreDozenalization = EntryKey (barlow (key :: _ "!.ignoreDozenalization"))

-- -- TODO: move renderingoptions also to cache?
getRenderingOptions :: forall m . MonadState State m => MonadEffect m => m RenderingOptions
getRenderingOptions = do
  tabNormalizationEnabled <- read cachedTabNormalizationEnabled
  tabDozenalizationEnabled <- read cachedTabDozenalizationEnabled
  chordDozenalizationEnabled <- read cachedChordDozenalizationEnabled
  ignoreDozenalization <- read cachedIgnoreDozenalization
  transposition <- read cachedTransposition
  pure $ { dozenalizeTabs: tabDozenalizationEnabled && not ignoreDozenalization
  , dozenalizeChords: chordDozenalizationEnabled && not ignoreDozenalization
  , normalizeTabs: tabNormalizationEnabled
  , transposition: transposition }
