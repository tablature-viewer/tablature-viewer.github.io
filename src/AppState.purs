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

newtype SimpleCacheEntry a = SimpleCacheEntry (Maybe a)

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

instance (MonadState State m, MonadEffect m) => CacheStore m Transposition CachedTransposition where
  fetch _ = liftEffect $ getTranspositionFromUrl
  flush _ value = liftEffect $ setAppQueryString { transposition: value }

instance CacheInvalidator State CachedTransposition where
  invalidate = noInvalidate



newtype CachedTablatureText = CachedTablatureText (SimpleCacheEntry String)

-- This works because the newtype is the last typeparameter in the list
derive newtype instance CacheEntry String CachedTablatureText

instance CacheDefault String CachedTablatureText where
  default _ = ""

instance (MonadState State m, MonadEffect m) => CacheStore m String CachedTablatureText where
  fetch _ = liftEffect $ getTablatureTextFromUrl <#> Just
  flush _ value = liftEffect $ saveTablatureToUrl value

instance CacheInvalidator State CachedTablatureText where
  invalidate _ state = purge (Proxy :: Proxy ParseResult) _parseResult state


_tablatureText :: Lens' State CachedTablatureText
_tablatureText = barlow (key :: _ "!.tablatureText")


newtype ParseResult = ParseResult TablatureDocument
type CachedParseResult = SimpleCacheEntry ParseResult

instance CacheDefault ParseResult CachedParseResult where
  default _ = ParseResult Nil

instance (MonadState State m, MonadEffect m) => CacheStore m ParseResult CachedParseResult where
  fetch c = do
    -- pure $ Just $ ParseResult Nil
    tablatureText <- getM _tablatureText
    pure $ ParseResult <$> tryParseTablature tablatureText
  flush _ value = pure unit

instance CacheInvalidator State CachedParseResult where
  invalidate = noInvalidate

_parseResult :: Lens' State CachedParseResult
_parseResult = barlow (key :: _ "!.parseResult")


initialState :: forall input . input -> State
initialState _ = State
  { mode: EditMode
  , loading: false
  , scrollTop: 0.0
  , autoscroll: false
  , autoscrollTimer: Nothing
  , autoscrollSpeed: Normal
  , tablatureText: CachedTablatureText (SimpleCacheEntry Nothing)
  , parseResult: SimpleCacheEntry Nothing
  -- , rewriteResult: NoValue
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


-- instance appState :: AppCache AppCacheState where
-- getTransposition = do
--   state <- H.get
--   pure $ view (key :: _ "cache!.transposition") state
-- setTransposition transposition = modify _ { transposition = transposition, rewriteResult = NoValue }
-- peekTransposition state = peekOrDefault defaultTransposition $ view (key :: _ "cache!.transposition") state
-- defaultTransposition = 

{-}
getRewriteResult = do
  state <- H.get
  case view (key :: _ "cache!.rewriteResult") state of
    Cached result -> pure result
    NoValue -> do
      parseResult <- getParseResult
      result <- pure $ rewriteTablatureDocument (peekRenderingOptions state) parseResult
      setRewriteResult result
      pure result
setRewriteResult rewriteResult =
  modifyCache _ { rewriteResult = Cached rewriteResult }
peekRewriteResult state = peekOrDefault Nil $ view (key :: _ "cache!.rewriteResult") state

getParseResult :: forall output m . MonadEffect m => H.HalogenM State Action () output m TablatureDocument
getParseResult = do
  state <- H.get
  case view (key :: _ "cache!.parseResult") state of
    Cached result -> pure result
    NoValue -> do
      tablatureText <- getTablatureText
      result <- pure $ parseTablature tablatureText
      setParseResult result
      pure result
setParseResult parseResult =
  modifyCache _ { parseResult = Cached parseResult }
peekParseResult state = peekOrDefault Nil $ view (key :: _ "cache!.parseResult") state

getTablatureText = do
  state <- H.get
  case view (key :: _ "cache!.tablatureText") state of
    Cached result -> pure result
    NoValue -> do
      tablatureText <- H.liftEffect getTablatureTextFromUrl
      setTablatureText tablatureText
      pure tablatureText
setTablatureText text = do
  H.liftEffect $ saveTablatureToUrl text
  modifyCache _ { tablatureText = Cached text, parseResult = NoValue, rewriteResult = NoValue, tablatureTitle = NoValue }
peekTablatureText state = peekOrDefault "" $ view (key :: _ "cache!.tablatureText") state

getTablatureTitle = do
  state <- H.get
  case view (key :: _ "cache!.tablatureTitle") state of
    Cached result -> pure result
    NoValue -> do
      tablatureDocument <- getRewriteResult
      title <- pure $ fromMaybe defaultTablatureTitle $ getTitle tablatureDocument
      modifyCache _ { tablatureTitle = Cached title }
      pure title
peekTablatureTitle state = peekOrDefault defaultTablatureTitle $ view (key :: _ "cache!.tablatureTitle") state

getTabNormalizationEnabled :: forall output m . MonadEffect m => H.HalogenM State Action () output m Boolean
getTabNormalizationEnabled = H.get >>= \state -> pure $ view (key :: _ "cache!.tabNormalizationEnabled") state
peekTabNormalizationEnabled :: State -> Boolean
peekTabNormalizationEnabled state = peekOrDefault defaultTabNormalizationEnabled $ view (key :: _ "cache!.tabNormalizationEnabled") state
toggleTabNormalizationEnabled :: forall output m . MonadEffect m => H.HalogenM State Action () output m Unit
toggleTabNormalizationEnabled = do
  state <- H.get
  modifyCache _ { tabNormalizationEnabled = Cached $ not (peekTabNormalizationEnabled state), rewriteResult = NoValue }
defaultTabNormalizationEnabled = true

getTabDozenalizationEnabled :: forall output m . MonadEffect m => H.HalogenM State Action () output m Boolean
getTabDozenalizationEnabled = H.get >>= \state -> pure $ view (key :: _ "cache!.tabDozenalizationEnabled") state
peekTabDozenalizationEnabled :: State -> Boolean
peekTabDozenalizationEnabled state = peekOrDefault defaultTabDozenalizationEnabled $ view (key :: _ "cache!.tabDozenalizationEnabled") state
toggleTabDozenalizationEnabled :: forall output m . MonadEffect m => H.HalogenM State Action () output m Unit
toggleTabDozenalizationEnabled = do
  state <- H.get
  modifyCache _ { tabDozenalizationEnabled = Cached $ not (peekTabNormalizationEnabled state), rewriteResult = NoValue }
defaultTabDozenalizationEnabled = false

-- TODO: abstract over boiler plate: the only variables are the lens, the default value and the storage getter
viewChordDozenalizationEnabled = view (key :: _ "cache!.chordDozenalizationEnabled")
_chordDozenalizationEnabled = barlow (key :: _ "cache!.chordDozenalizationEnabled")
getChordDozenalizationEnabled :: forall output m . MonadEffect m => H.HalogenM State Action () output m Boolean
getChordDozenalizationEnabled = do
  state <- H.get
  case viewChordDozenalizationEnabled state of
    Cached result -> pure result
    NoValue -> do
      value <- H.liftEffect $ getLocalStorageBooleanWithDefault defaultChordDozenalizationEnabled localStorageKeyChordDozenalizationEnabled
      if value /= peekChordDozenalizationEnabled state
      then modifyCache _ { chordDozenalizationEnabled = Cached value, rewriteResult = NoValue }
      else pure unit
      pure value
peekChordDozenalizationEnabled state = peekOrDefault defaultChordDozenalizationEnabled $ viewChordDozenalizationEnabled state
toggleChordDozenalizationEnabled :: forall output m . MonadEffect m => H.HalogenM State Action () output m Unit
toggleChordDozenalizationEnabled = do
  state <- H.get
  modifyCache _ { chordDozenalizationEnabled = not (peekChordDozenalizationEnabled state), rewriteResult = NoValue }
defaultChordDozenalizationEnabled = false

-- TODO also check if tablature contains ↊ or ↋
getIgnoreDozenalization = do
  title <- getTablatureTitle
  pure $ test (unsafeRegex "dozenal" ignoreCase) title
peekIgnoreDozenalization state = peekOrDefault defaultIgnoreDozenalization $ view (key :: _ "cache!.ignoreDozenalization") state
defaultIgnoreDozenalization = false

      -- maybeTabNormalizationEnabled <- H.liftEffect $ getLocalStorageBoolean localStorageKeyTabNormalizationEnabled
      -- tabNormalizationEnabled <- pure $ fromMaybe originalState.tabNormalizationEnabled maybeTabNormalizationEnabled
      -- maybeTabDozenalizationEnabled <- H.liftEffect $ getLocalStorageBoolean localStorageKeyTabDozenalizationEnabled
      -- tabDozenalizationEnabled <- pure $ fromMaybe originalState.tabDozenalizationEnabled maybeTabDozenalizationEnabled

      -- maybeTransposition <- H.liftEffect $ getTranspositionFromUrl
      -- transposition <- pure $ fromMaybe originalState.transposition maybeTransposition
      -- H.modify_ _ { scrollTop = originalState.scrollTop, tabNormalizationEnabled = tabNormalizationEnabled, tabDozenalizationEnabled = tabDozenalizationEnabled, chordDozenalizationEnabled = chordDozenalizationEnabled, transposition = transposition}

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
