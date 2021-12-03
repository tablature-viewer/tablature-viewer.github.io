module AppState where

import Cache
import Prelude

import AppUrl (getTablatureTextFromUrl, getTranspositionFromUrl, saveTablatureToUrl, setAppQueryString)
import AutoscrollSpeed (AutoscrollSpeed(..))
import Control.Monad.State (class MonadState, StateT(..))
import Control.Monad.State as MonadState
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Lens (Lens')
import Data.Lens.Barlow (barlow, key)
import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String.Regex (test)
import Data.String.Regex.Flags (ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Timer (IntervalId)
import LocalStorage (getLocalStorageBoolean, setLocalStorageBoolean)
import TablatureDocument (RenderingOptions, TablatureDocument, Transposition(..), getTitle)
import TablatureParser (tryParseTablature)
import TablatureRewriter (rewriteTablatureDocument)

-- TODO: rename to AppAction?
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

data Mode = ViewMode | EditMode

newtype State m = State (StateRecord m)
type StateRecord m =
  { mode :: Mode
  , autoscrollTimer :: Maybe IntervalId
  , autoscrollSpeed :: AutoscrollSpeed
  , autoscroll :: Boolean
  -- Store the scrollTop in the state before actions so we can restore the expected scrollTop when switching views
  , scrollTop :: Number
  , loading :: Boolean
  , tablatureText :: AppStateReadWriteCacheUnit String m
  , parseResult :: AppStateReadCacheUnit TablatureDocument m
  , rewriteResult :: AppStateReadCacheUnit TablatureDocument m
  , tablatureTitle :: AppStateReadCacheUnit String m
  , tabNormalizationEnabled :: AppStateReadWriteCacheUnit Boolean m
  , tabDozenalizationEnabled :: AppStateReadWriteCacheUnit Boolean m
  , chordDozenalizationEnabled :: AppStateReadWriteCacheUnit Boolean m
  -- For tabs that are already dozenal themselves we want to ignore any dozenalization settings
  , ignoreDozenalization :: AppStateReadCacheUnit Boolean m
  , transposition :: AppStateReadWriteCacheUnit Transposition m
  }
derive instance Newtype (State m) _

-- modifyState :: forall m . MonadState (State m) m => (StateRecord m -> StateRecord m) -> m Unit
modifyState f = MonadState.modify_ \(State s) -> State (f s)

-- getState :: forall m . MonadState (State m) m => m (StateRecord m)
getState = do
  State state <- MonadState.get
  pure state


type AppStateReadWriteCacheUnit a m = ReadWriteCacheUnit (State m) a () m
type AppStateReadWriteCacheUnit' a = forall m . MonadEffect m => MonadState (State m) m => AppStateReadWriteCacheUnit a m
-- type AppStateReadWriteCacheKey' a = forall m . MonadEffect m => MonadState (State m) m => ReadWriteCacheKey (State m) a () m

type AppStateReadCacheUnit a m = ReadableCacheUnit (State m) a () m
type AppStateReadCacheUnit' a = forall m . MonadEffect m => MonadState (State m) m => AppStateReadCacheUnit a m
-- type AppStateReadCacheKey' a = forall m . MonadEffect m => MonadState (State m) m => ReadableCacheKey (State m) a () m

initialState :: forall input m . MonadEffect m => MonadState (State m) m => input -> State m
initialState _ = State
  { mode: EditMode
  , loading: false
  , scrollTop: 0.0
  , autoscroll: false
  , autoscrollTimer: Nothing
  , autoscrollSpeed: Normal
  , tablatureText: cachedTablatureText
  , parseResult: cachedParseResult
  , rewriteResult: cachedRewriteResult
  , tablatureTitle: cachedTablatureTitle
  , tabNormalizationEnabled: cachedTabNormalizationEnabled
  , tabDozenalizationEnabled: cachedTabDozenalizationEnabled
  , chordDozenalizationEnabled: cachedChordDozenalizationEnabled
  , ignoreDozenalization: cachedIgnoreDozenalization
  , transposition: cachedTransposition
  }

cachedTransposition :: AppStateReadWriteCacheUnit' Transposition
cachedTransposition = readWriteCache
  { default: Transposition 0
  , flush: Flush $ \value -> liftEffect $ setAppQueryString { transposition: value }
  , fetch: Fetch $ liftEffect getTranspositionFromUrl
  }
-- _transposition :: AppStateReadWriteCacheKey' Transposition
_transposition = barlow (key :: _ "!.transposition")

cachedTablatureText :: AppStateReadWriteCacheUnit' String
cachedTablatureText = readWriteCache
  { default: ""
  , flush: Flush $ \value -> liftEffect $ saveTablatureToUrl value
  , fetch: Fetch $ liftEffect $ getTablatureTextFromUrl <#> Just
  }
-- _tablatureText :: AppStateReadWriteCacheKey' String
_tablatureText = barlow (key :: _ "!.tablatureText")

cachedParseResult :: AppStateReadCacheUnit' TablatureDocument
cachedParseResult = readonlyCache
  { default: Nil
  , fetch: Fetch $ do
      tablatureText <- depend _parseResult _tablatureText
      pure $ tryParseTablature tablatureText
  }
-- _parseResult :: AppStateReadCacheKey' TablatureDocument
_parseResult = barlow (key :: _ "!.parseResult")

cachedRewriteResult :: AppStateReadCacheUnit' TablatureDocument
cachedRewriteResult = readonlyCache
  { default: Nil
  , fetch: Fetch $ do
      parseResult <- depend _rewriteResult _parseResult
      renderingOptions <- getRenderingOptions
      pure $ Just $ rewriteTablatureDocument renderingOptions parseResult
  }
-- _rewriteResult :: AppStateReadCacheKey' TablatureDocument
_rewriteResult = barlow (key :: _ "!.rewriteResult")

cachedTablatureTitle :: AppStateReadCacheUnit' String
cachedTablatureTitle = readonlyCache
  { default: "Tab viewer"
  , fetch: Fetch $ do
      parseResult <- depend _tablatureTitle _parseResult
      pure $ getTitle parseResult
  }
-- _tablatureTitle :: AppStateReadCacheKey' String
_tablatureTitle = barlow (key :: _ "!.tablatureTitle")

cachedLocalStorageBoolean :: String -> Boolean -> AppStateReadWriteCacheUnit' Boolean
cachedLocalStorageBoolean localStorageKey default = readWriteCache
  { default: default
  , flush: Flush $ \value -> liftEffect $ setLocalStorageBoolean localStorageKey value
  , fetch: Fetch $ liftEffect $ getLocalStorageBoolean localStorageKey
  }

cachedTabNormalizationEnabled :: AppStateReadWriteCacheUnit' Boolean
cachedTabNormalizationEnabled = cachedLocalStorageBoolean "tabNormalizationEnabled" true
-- _tabNormalizationEnabled :: AppStateReadWriteCacheKey' Boolean
_tabNormalizationEnabled = barlow (key :: _ "!.tabNormalizationEnabled")

cachedTabDozenalizationEnabled :: AppStateReadWriteCacheUnit' Boolean
cachedTabDozenalizationEnabled = cachedLocalStorageBoolean "tabDozenalizationEnabled" false
-- _tabDozenalizationEnabled :: AppStateReadWriteCacheKey' Boolean
_tabDozenalizationEnabled = barlow (key :: _ "!.tabDozenalizationEnabled")

cachedChordDozenalizationEnabled :: AppStateReadWriteCacheUnit' Boolean
cachedChordDozenalizationEnabled = cachedLocalStorageBoolean "chordDozenalizationEnabled" false
-- _chordDozenalizationEnabled :: AppStateReadWriteCacheKey' Boolean
_chordDozenalizationEnabled = barlow (key :: _ "!.chordDozenalizationEnabled")

cachedIgnoreDozenalization :: AppStateReadCacheUnit' Boolean
cachedIgnoreDozenalization = readonlyCache
  { default: false
  , fetch: Fetch $ do
      tablatureTitle <- depend _ignoreDozenalization _tablatureTitle
      pure $ Just $ test (unsafeRegex "dozenal" ignoreCase) tablatureTitle
  }
-- _ignoreDozenalization :: AppStateReadCacheKey' Boolean
_ignoreDozenalization = barlow (key :: _ "!.ignoreDozenalization")

-- -- TODO: move renderingoptions also to cache?
getRenderingOptions :: forall m . MonadState (State m) m => MonadEffect m => m RenderingOptions
getRenderingOptions = do
  tabNormalizationEnabled <- read _tabNormalizationEnabled
  tabDozenalizationEnabled <- read _tabDozenalizationEnabled
  chordDozenalizationEnabled <- read _chordDozenalizationEnabled
  ignoreDozenalization <- read _ignoreDozenalization
  transposition <- read _transposition
  pure $ { dozenalizeTabs: tabDozenalizationEnabled && not ignoreDozenalization
  , dozenalizeChords: chordDozenalizationEnabled && not ignoreDozenalization
  , normalizeTabs: tabNormalizationEnabled
  , transposition: transposition }
