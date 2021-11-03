module AppState where

import Cache
import Prelude

import Type.Prelude (Proxy(..))
import AppUrl (getTablatureTextFromUrl, getTranspositionFromUrl, saveTablatureToUrl, setAppQueryString)
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
import LocalStorage (getLocalStorageBoolean, getLocalStorageBooleanWithDefault)
import TablatureDocument (RenderingOptions, TablatureDocument, Transposition(..), getTitle)
import TablatureParser (parseTablature, tryParseTablature)
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

data AutoscrollSpeed
  = Slowest
  | Slower
  | Slow
  | Normal
  | Fast
  | Fastest

instance showAutoscrollSpeed :: Show AutoscrollSpeed where
  show Slowest = "(0.2)"
  show Slower = "(0.4)"
  show Slow = "(0.6)"
  show Normal = "(1.0)"
  show Fast = "(2.0)"
  show Fastest = "(4.0)"

derive instance eqAutoscrollSpeed :: Eq AutoscrollSpeed
derive instance ordAutoscrollSpeed :: Ord AutoscrollSpeed
derive instance genericAutoscrollSpeed :: Generic AutoscrollSpeed _
instance enumAutoscrollSpeed :: Enum AutoscrollSpeed where
  succ = genericSucc
  pred = genericPred

speedToIntervalMs :: AutoscrollSpeed -> Int
speedToIntervalMs Slowest = 400
speedToIntervalMs Slower = 200
speedToIntervalMs Slow = 120
speedToIntervalMs Normal = 80
speedToIntervalMs Fast = 40
speedToIntervalMs Fastest = 40

speedToIntervalPixelDelta :: AutoscrollSpeed -> Int
speedToIntervalPixelDelta Slowest = 1
speedToIntervalPixelDelta Slower = 1
speedToIntervalPixelDelta Slow = 1
speedToIntervalPixelDelta Normal = 1
speedToIntervalPixelDelta Fast = 1
speedToIntervalPixelDelta Fastest = 2


newtype State = State
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
  -- , rewriteResult :: CacheRecord (State m) m TablatureDocument
  -- , tablatureTitle :: CacheRecord (State m) m String
  -- , tabNormalizationEnabled :: CacheRecord (State m) m Boolean
  -- , tabDozenalizationEnabled :: CacheRecord (State m) m Boolean
  -- , chordDozenalizationEnabled :: CacheRecord (State m) m Boolean
  , transposition :: CachedTransposition
  -- For tabs that are already dozenal themselves we want to ignore any dozenalization settings
  -- , ignoreDozenalization :: Cache (State m) m Boolean
  }
derive instance Newtype State _

data SimpleCacheEntry a = SimpleCacheEntry (Maybe a)

instance CacheEntry a (SimpleCacheEntry a) where
  getCacheValue (SimpleCacheEntry c) = c
  setCacheValue _ newValue = SimpleCacheEntry newValue


-- FIXME: why doesn't this work?
-- class NoInvalidator c
-- instance (NoInvalidator c) => CacheInvalidator State c where
--   invalidate _ state = state


type CachedTransposition = SimpleCacheEntry Transposition

instance CacheDefault Transposition CachedTransposition where
  default _ = Transposition 0

instance (MonadState State m, MonadEffect m) => Fetchable m Transposition CachedTransposition where
  fetch _ = liftEffect $ getTranspositionFromUrl

instance (MonadState State m, MonadEffect m) => Flushable m Transposition CachedTransposition where
  flush _ value = liftEffect $ setAppQueryString { transposition: value }

instance CacheInvalidator State CachedTransposition where
  invalidate = noInvalidate



newtype CachedTablatureText = CachedTablatureText (SimpleCacheEntry String)
derive newtype instance CacheEntry String CachedTablatureText

instance CacheDefault String CachedTablatureText where
  default _ = ""

instance (MonadState State m, MonadEffect m) => Fetchable m String CachedTablatureText where
  fetch _ = liftEffect $ getTablatureTextFromUrl <#> Just

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
  fetch c = do
    tablatureText <- getM _tablatureText
    pure $ tryParseTablature tablatureText

instance (MonadState State m, MonadEffect m) => Flushable m TablatureDocument CachedParseResult where
  flush _ value = pure unit

instance CacheInvalidator State CachedParseResult where
  invalidate = noInvalidate

_parseResult :: Lens' State CachedParseResult
_parseResult = barlow (key :: _ "!.parseResult")


newtype CachedRewriteResult = CachedRewriteResult (SimpleCacheEntry TablatureDocument)
derive newtype instance CacheEntry TablatureDocument CachedRewriteResult 

instance CacheDefault TablatureDocument CachedRewriteResult where
  default _ = Nil

instance (MonadState State m, MonadEffect m) => Fetchable m TablatureDocument CachedRewriteResult where
  fetch c = do
    pure $ Just Nil
    -- parseResult <- getM _parseResult
    -- pure $ rewriteTablatureDocument parseResult

instance (MonadState State m, MonadEffect m) => Flushable m TablatureDocument CachedRewriteResult where
  flush _ value = pure unit

instance CacheInvalidator State CachedRewriteResult where
  invalidate = noInvalidate

_rewriteResult :: Lens' State CachedRewriteResult
_rewriteResult = barlow (key :: _ "!.rewriteResult")


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
  -- , tablatureTitle: Cached defaultTablatureTitle
  -- , tabNormalizationEnabled: NoValue
  -- , tabDozenalizationEnabled: NoValue
  -- , chordDozenalizationEnabled: NoValue
  -- , ignoreDozenalization: NoValue
  , transposition: SimpleCacheEntry Nothing
  }

cacheState = do
  -- _ <- getTablatureTitle
  pure unit


{-}
defaultTablatureTitle :: String
defaultTablatureTitle = "Tab viewer"

-- getRenderingOptions  :: forall output m . MonadEffect m => H.HalogenM State Action () output m RenderingOptions
getRenderingOptions = do
  tabNormalizationEnabled <- getTabNormalizationEnabled
  tabDozenalizationEnabled <- getTabDozenalizationEnabled
  chordDozenalizationEnabled <- getChordDozenalizationEnabled
  ignoreDozenalization <- getIgnoreDozenalization
  transposition <- getTransposition
  pure $ { dozenalizeTabs: tabDozenalizationEnabled && not ignoreDozenalization
  , dozenalizeChords: chordDozenalizationEnabled && not ignoreDozenalization
  , normalizeTabs: tabNormalizationEnabled
  , transposition: transposition }

peekRenderingOptions :: forall c . State -> RenderingOptions
peekRenderingOptions state =
  { dozenalizeTabs: tabDozenalizationEnabled && not ignoreDozenalization
  , dozenalizeChords: chordDozenalizationEnabled && not ignoreDozenalization
  , normalizeTabs: tabNormalizationEnabled
  , transposition: transposition }
  where
  tabNormalizationEnabled = peekTabNormalizationEnabled state
  tabDozenalizationEnabled = peekTabDozenalizationEnabled state
  chordDozenalizationEnabled = peekChordDozenalizationEnabled state
  ignoreDozenalization = peekIgnoreDozenalization state
  transposition = peekTransposition state


localStorageKeyTabNormalizationEnabled :: String
localStorageKeyTabNormalizationEnabled = "tabNormalizationEnabled"

localStorageKeyTabDozenalizationEnabled :: String
localStorageKeyTabDozenalizationEnabled = "tabDozenalizationEnabled"

localStorageKeyChordDozenalizationEnabled :: String
localStorageKeyChordDozenalizationEnabled = "ChordDozenalizationEnabled"

-}
