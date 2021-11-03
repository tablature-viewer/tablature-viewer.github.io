module AppState where

import Cache
import Prelude
import DebugUtils

import AppUrl (getTablatureTextFromUrl, getTranspositionFromUrl, saveTablatureToUrl, setAppQueryString)
import AutoscrollSpeed (AutoscrollSpeed(..))
import Control.Monad.State (class MonadState)
import Control.Monad.State as MonadState
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Functor.App (App(..))
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Barlow (barlow, key)
import Data.Lens.Barlow.Helpers (view, over)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.String.Regex (test)
import Data.String.Regex.Flags (ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Timer (IntervalId)
import Halogen as H
import LocalStorage (getLocalStorageBoolean, getLocalStorageBooleanWithDefault, setLocalStorageBoolean)
import TablatureDocument (RenderingOptions, TablatureDocument, Transposition(..), getTitle)
import TablatureParser (parseTablature, tryParseTablature)
import TablatureRewriter (rewriteTablatureDocument)
import Type.Prelude (Proxy(..))

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
  , tablatureText :: CachedTablatureText
  , parseResult :: CachedParseResult
  , rewriteResult :: CachedRewriteResult
  , tablatureTitle :: CachedTablatureTitle
  , tabNormalizationEnabled :: CachedTabNormalizationEnabled
  , tabDozenalizationEnabled :: CachedTabDozenalizationEnabled
  , chordDozenalizationEnabled :: CachedChordDozenalizationEnabled
  -- For tabs that are already dozenal themselves we want to ignore any dozenalization settings
  , ignoreDozenalization :: CachedIgnoreDozenalization
  , transposition :: CachedTransposition
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
  , tablatureText: CachedTablatureText (SimpleCacheEntry Nothing)
  , parseResult: CachedParseResult (SimpleCacheEntry Nothing)
  , rewriteResult: CachedRewriteResult (SimpleCacheEntry Nothing)
  , tablatureTitle: CachedTablatureTitle (SimpleCacheEntry Nothing)
  , tabNormalizationEnabled: CachedTabNormalizationEnabled (SimpleCacheEntry Nothing)
  , tabDozenalizationEnabled: CachedTabDozenalizationEnabled (SimpleCacheEntry Nothing)
  , chordDozenalizationEnabled: CachedChordDozenalizationEnabled (SimpleCacheEntry Nothing)
  , ignoreDozenalization: CachedIgnoreDozenalization (SimpleCacheEntry Nothing)
  , transposition: SimpleCacheEntry Nothing
  }

newtype SimpleCacheEntry a = SimpleCacheEntry (Maybe a)
derive newtype instance (Show a) => Show (SimpleCacheEntry a)

instance CacheEntry a (SimpleCacheEntry a) where
  getCacheValue (SimpleCacheEntry c) = c
  setCacheValue _ newValue = SimpleCacheEntry newValue


type CachedTransposition = SimpleCacheEntry Transposition

instance CacheDefault Transposition CachedTransposition where
  default _ = Transposition 0

instance (MonadState State m, MonadEffect m) => Fetchable m Transposition CachedTransposition where
  fetch _ = liftEffect $ getTranspositionFromUrl

instance (MonadState State m, MonadEffect m) => Flushable m Transposition CachedTransposition where
  flush _ value = liftEffect $ setAppQueryString { transposition: value }

instance CacheInvalidator State CachedTransposition where
  invalidate = noInvalidate

_transposition :: Lens' State CachedTransposition
_transposition = barlow (key :: _ "!.transposition")


newtype CachedTablatureText = CachedTablatureText (SimpleCacheEntry String)
derive newtype instance CacheEntry String CachedTablatureText

instance CacheDefault String CachedTablatureText where
  default _ = ""

instance (MonadState State m, MonadEffect m) => Fetchable m String CachedTablatureText where
  fetch _ = do
    liftEffect $ getTablatureTextFromUrl <#> Just

instance (MonadState State m, MonadEffect m) => Flushable m String CachedTablatureText where
  flush _ value = liftEffect $ saveTablatureToUrl value

instance CacheInvalidator State CachedTablatureText where
  invalidate _ state = purge (Proxy :: Proxy TablatureDocument) _parseResult state

_tablatureText :: Lens' State CachedTablatureText
_tablatureText = barlow (key :: _ "!.tablatureText")


newtype CachedParseResult = CachedParseResult (SimpleCacheEntry TablatureDocument)
derive newtype instance CacheEntry TablatureDocument CachedParseResult 

instance CacheDefault TablatureDocument CachedParseResult where
  default _ = Nil

instance (MonadState State m, MonadEffect m) => Fetchable m TablatureDocument CachedParseResult where
  fetch _ = do
    tablatureText <- getM _tablatureText
    pure $ tryParseTablature tablatureText

instance CacheInvalidator State CachedParseResult where
  invalidate = noInvalidate

_parseResult :: Lens' State CachedParseResult
_parseResult = barlow (key :: _ "!.parseResult")


newtype CachedRewriteResult = CachedRewriteResult (SimpleCacheEntry TablatureDocument)
derive newtype instance CacheEntry TablatureDocument CachedRewriteResult 

instance CacheDefault TablatureDocument CachedRewriteResult where
  default _ = Nil

instance (MonadState State m, MonadEffect m) => Fetchable m TablatureDocument CachedRewriteResult where
  fetch _ = do
    parseResult <- getM _parseResult
    renderingOptions <- getRenderingOptions
    pure $ Just $ rewriteTablatureDocument renderingOptions parseResult

instance CacheInvalidator State CachedRewriteResult where
  invalidate = noInvalidate

_rewriteResult :: Lens' State CachedRewriteResult
_rewriteResult = barlow (key :: _ "!.rewriteResult")



newtype CachedTablatureTitle = CachedTablatureTitle (SimpleCacheEntry String)
derive newtype instance CacheEntry String CachedTablatureTitle 

instance CacheDefault String CachedTablatureTitle where
  default _ = "Tab viewer"

instance (MonadState State m, MonadEffect m) => Fetchable m String CachedTablatureTitle where
  fetch _ = do
    rewriteResult <- getM _rewriteResult
    pure $ getTitle rewriteResult

instance CacheInvalidator State CachedTablatureTitle where
  invalidate = noInvalidate

_tablatureTitle :: Lens' State CachedTablatureTitle
_tablatureTitle = barlow (key :: _ "!.tablatureTitle")




newtype CachedTabNormalizationEnabled = CachedTabNormalizationEnabled (SimpleCacheEntry Boolean)
derive newtype instance CacheEntry Boolean CachedTabNormalizationEnabled 

instance CacheDefault Boolean CachedTabNormalizationEnabled where
  default _ = false

instance (MonadState State m, MonadEffect m) => Fetchable m Boolean CachedTabNormalizationEnabled where
  fetch _ = liftEffect $ getLocalStorageBoolean localStorageKeyTabNormalizationEnabled

instance (MonadState State m, MonadEffect m) => Flushable m Boolean CachedTabNormalizationEnabled where
  flush _ value = liftEffect $ setLocalStorageBoolean localStorageKeyTabNormalizationEnabled value

instance CacheInvalidator State CachedTabNormalizationEnabled where
  invalidate = noInvalidate

_tabNormalizationEnabled :: Lens' State CachedTabNormalizationEnabled
_tabNormalizationEnabled = barlow (key :: _ "!.tabNormalizationEnabled")

localStorageKeyTabNormalizationEnabled :: String
localStorageKeyTabNormalizationEnabled = "tabNormalizationEnabled"


newtype CachedTabDozenalizationEnabled = CachedTabDozenalizationEnabled (SimpleCacheEntry Boolean)
derive newtype instance CacheEntry Boolean CachedTabDozenalizationEnabled 

instance CacheDefault Boolean CachedTabDozenalizationEnabled where
  default _ = false

instance (MonadState State m, MonadEffect m) => Fetchable m Boolean CachedTabDozenalizationEnabled where
  fetch _ = liftEffect $ getLocalStorageBoolean localStorageKeyTabDozenalizationEnabled

instance (MonadState State m, MonadEffect m) => Flushable m Boolean CachedTabDozenalizationEnabled where
  flush _ value = liftEffect $ setLocalStorageBoolean localStorageKeyTabDozenalizationEnabled value

instance CacheInvalidator State CachedTabDozenalizationEnabled where
  invalidate = noInvalidate

_tabDozenalizationEnabled :: Lens' State CachedTabDozenalizationEnabled
_tabDozenalizationEnabled = barlow (key :: _ "!.tabDozenalizationEnabled")

localStorageKeyTabDozenalizationEnabled :: String
localStorageKeyTabDozenalizationEnabled = "tabDozenalizationEnabled"


newtype CachedChordDozenalizationEnabled = CachedChordDozenalizationEnabled (SimpleCacheEntry Boolean)
derive newtype instance CacheEntry Boolean CachedChordDozenalizationEnabled 

instance CacheDefault Boolean CachedChordDozenalizationEnabled where
  default _ = false

instance (MonadState State m, MonadEffect m) => Fetchable m Boolean CachedChordDozenalizationEnabled where
  fetch _ = liftEffect $ getLocalStorageBoolean localStorageKeyChordDozenalizationEnabled

instance (MonadState State m, MonadEffect m) => Flushable m Boolean CachedChordDozenalizationEnabled where
  flush _ value = liftEffect $ setLocalStorageBoolean localStorageKeyChordDozenalizationEnabled value

instance CacheInvalidator State CachedChordDozenalizationEnabled where
  invalidate = noInvalidate

_chordDozenalizationEnabled :: Lens' State CachedChordDozenalizationEnabled
_chordDozenalizationEnabled = barlow (key :: _ "!.chordDozenalizationEnabled")

localStorageKeyChordDozenalizationEnabled :: String
localStorageKeyChordDozenalizationEnabled = "chordDozenalizationEnabled"


newtype CachedIgnoreDozenalization = CachedIgnoreDozenalization (SimpleCacheEntry Boolean)
derive newtype instance CacheEntry Boolean CachedIgnoreDozenalization 

instance CacheDefault Boolean CachedIgnoreDozenalization where
  default _ = false

instance (MonadState State m, MonadEffect m) => Fetchable m Boolean CachedIgnoreDozenalization where
  fetch _ = do
    pure Nothing -- TODO

instance CacheInvalidator State CachedIgnoreDozenalization where
  invalidate = noInvalidate

_ignoreDozenalization :: Lens' State CachedIgnoreDozenalization
_ignoreDozenalization = barlow (key :: _ "!.ignoreDozenalization")


-- TODO: move renderingoptions also to cache?
getRenderingOptions  :: forall m . MonadState State m => MonadEffect m => m RenderingOptions
getRenderingOptions = do
  tabNormalizationEnabled <- getM _tabNormalizationEnabled
  tabDozenalizationEnabled <- getM _tabDozenalizationEnabled
  chordDozenalizationEnabled <- getM _chordDozenalizationEnabled
  ignoreDozenalization <- getM _ignoreDozenalization
  transposition <- getM _transposition
  pure $ { dozenalizeTabs: tabDozenalizationEnabled && not ignoreDozenalization
  , dozenalizeChords: chordDozenalizationEnabled && not ignoreDozenalization
  , normalizeTabs: tabNormalizationEnabled
  , transposition: transposition }

-- peekRenderingOptions :: forall c . State -> RenderingOptions
-- peekRenderingOptions state =
--   { dozenalizeTabs: tabDozenalizationEnabled && not ignoreDozenalization
--   , dozenalizeChords: chordDozenalizationEnabled && not ignoreDozenalization
--   , normalizeTabs: tabNormalizationEnabled
--   , transposition: transposition }
--   where
--   tabNormalizationEnabled = peekTabNormalizationEnabled state
--   tabDozenalizationEnabled = peekTabDozenalizationEnabled state
--   chordDozenalizationEnabled = peekChordDozenalizationEnabled state
--   ignoreDozenalization = peekIgnoreDozenalization state
--   transposition = peekTransposition state
