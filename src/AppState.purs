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
import TablatureDocument (NoteLetterPrimitive(..), RenderingOptions, TablatureDocument, Transposition(..), getTitle)
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

newtype State = State StateRecord
type StateRecord =
  { mode :: Mode
  , autoscrollTimer :: Maybe IntervalId
  , autoscrollSpeed :: AutoscrollSpeed
  , autoscroll :: Boolean
  -- Store the scrollTop in the state before actions so we can restore the expected scrollTop when switching views
  , scrollTop :: Number
  , loading :: Boolean
  , tablatureText :: CacheUnit State String
  , parseResult :: CacheUnit State TablatureDocument
  , rewriteResult :: CacheUnit State TablatureDocument
  , tablatureTitle :: CacheUnit State String
  , tabNormalizationEnabled :: CacheUnit State Boolean
  , tabDozenalizationEnabled :: CacheUnit State Boolean
  , chordDozenalizationEnabled :: CacheUnit State Boolean
  -- For tabs that are already dozenal themselves we want to ignore any dozenalization settings
  , ignoreDozenalization :: CacheUnit State Boolean
  , transposition :: CacheUnit State Transposition
  }
derive instance Newtype State _

-- modifyState :: forall m . MonadState (State m) m => (StateRecord m -> StateRecord m) -> m Unit
modifyState f = MonadState.modify_ \(State s) -> State (f s)

-- getState :: forall m . MonadState (State m) m => m (StateRecord m)
getState = do
  State state <- MonadState.get
  pure state


type AppStateReadWriteCacheDef a m = ReadWriteCacheDef State a () m
type AppStateReadWriteCacheDef' a = forall m . MonadEffect m => MonadState State m => AppStateReadWriteCacheDef a m
-- type AppStateReadWriteCacheKey' a = forall m . MonadEffect m => MonadState State m => ReadWriteCacheKey State a () m

type AppStateReadCacheDef a m = ReadableCacheDef State a () m
type AppStateReadCacheDef' a = forall m . MonadEffect m => MonadState State m => AppStateReadCacheDef a m
-- type AppStateReadCacheKey' a = forall m . MonadEffect m => MonadState State m => ReadableCacheKey State a () m

initialState :: forall input m . MonadEffect m => MonadState State m => input -> State
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
  { cache: _transposition
  , flush: Flush $ \value -> liftEffect $ setAppQueryString { transposition: value }
  , fetch: Fetch $ liftEffect getTranspositionFromUrl
  }
_transposition :: CacheKey State Transposition
_transposition = CacheKey (barlow (key :: _ "!.transposition"))

cachedTablatureText :: AppStateReadWriteCacheDef' String
cachedTablatureText =
  { cache: _tablatureText
  , flush: Flush $ \value -> liftEffect $ saveTablatureToUrl value
  , fetch: Fetch $ liftEffect $ getTablatureTextFromUrl <#> Just
  }
_tablatureText :: CacheKey State String
_tablatureText = CacheKey (barlow (key :: _ "!.tablatureText"))

cachedParseResult :: AppStateReadCacheDef' TablatureDocument
cachedParseResult =
  { cache: cacheKey
  , fetch: Fetch $ do
      tablatureText <- depend cacheKey cachedTablatureText
      pure $ tryParseTablature tablatureText
  } where cacheKey = _parseResult 
_parseResult :: CacheKey State TablatureDocument
_parseResult = CacheKey (barlow (key :: _ "!.parseResult"))

cachedRewriteResult :: AppStateReadCacheDef' TablatureDocument
cachedRewriteResult =
  { cache: cacheKey
  , fetch: Fetch $ do
      parseResult <- depend cacheKey cachedParseResult
      renderingOptions <- getRenderingOptions
      pure $ Just $ rewriteTablatureDocument renderingOptions parseResult
  } where cacheKey = _rewriteResult 
_rewriteResult :: CacheKey State TablatureDocument
_rewriteResult = CacheKey (barlow (key :: _ "!.rewriteResult"))

cachedTablatureTitle :: AppStateReadCacheDef' String
cachedTablatureTitle =
  { cache: cacheKey
  , fetch: Fetch $ do
      parseResult <- depend cacheKey cachedParseResult
      pure $ getTitle parseResult
  } where cacheKey = _tablatureTitle 
_tablatureTitle :: CacheKey State String
_tablatureTitle = CacheKey (barlow (key :: _ "!.tablatureTitle"))

cachedLocalStorageBoolean :: String -> CacheKey State Boolean -> AppStateReadWriteCacheDef' Boolean
cachedLocalStorageBoolean localStorageKey _key =
  { cache: _key
  , flush: Flush $ \value -> liftEffect $ setLocalStorageBoolean localStorageKey value
  , fetch: Fetch $ liftEffect $ getLocalStorageBoolean localStorageKey
  }

cachedTabNormalizationEnabled :: AppStateReadWriteCacheDef' Boolean
cachedTabNormalizationEnabled = cachedLocalStorageBoolean "tabNormalizationEnabled" _tabNormalizationEnabled 
_tabNormalizationEnabled :: CacheKey State Boolean
_tabNormalizationEnabled = CacheKey (barlow (key :: _ "!.tabNormalizationEnabled"))

cachedTabDozenalizationEnabled :: AppStateReadWriteCacheDef' Boolean
cachedTabDozenalizationEnabled = cachedLocalStorageBoolean "tabDozenalizationEnabled" _tabDozenalizationEnabled 
_tabDozenalizationEnabled :: CacheKey State Boolean
_tabDozenalizationEnabled = CacheKey (barlow (key :: _ "!.tabDozenalizationEnabled"))

cachedChordDozenalizationEnabled :: AppStateReadWriteCacheDef' Boolean
cachedChordDozenalizationEnabled = cachedLocalStorageBoolean "chordDozenalizationEnabled" _chordDozenalizationEnabled 
_chordDozenalizationEnabled :: CacheKey State Boolean
_chordDozenalizationEnabled = CacheKey (barlow (key :: _ "!.chordDozenalizationEnabled"))

cachedIgnoreDozenalization :: AppStateReadCacheDef' Boolean
cachedIgnoreDozenalization =
  { cache: cacheKey
  , fetch: Fetch $ do
      tablatureTitle <- depend cacheKey cachedTablatureTitle
      pure $ Just $ test (unsafeRegex "dozenal" ignoreCase) tablatureTitle
  } where cacheKey = _ignoreDozenalization
_ignoreDozenalization :: CacheKey State Boolean
_ignoreDozenalization = CacheKey (barlow (key :: _ "!.ignoreDozenalization"))

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
