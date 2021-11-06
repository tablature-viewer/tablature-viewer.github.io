module AppState where

import Prelude

import AppUrl (getTablatureTextFromUrl, getTranspositionFromUrl, saveTablatureToUrl, setAppQueryString)
import AutoscrollSpeed (AutoscrollSpeed(..))
import Cache (class CacheDefault, class CacheEntry, class Dependable, class Fetchable, class Flushable, class Purgeable, getM, packPurgeableLens)
import Control.Monad.State (class MonadState)
import Data.Lens (Lens')
import Data.Lens.Barlow (barlow, key)
import Data.List (List(..), fromFoldable)
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
  setCacheValue _ newValue = SimpleCacheEntry (Just newValue)
instance Purgeable (SimpleCacheEntry a) where
  purgeCacheValue _ = SimpleCacheEntry Nothing
  hasCacheValue (SimpleCacheEntry Nothing) = false
  hasCacheValue _ = true

type CachedTransposition = SimpleCacheEntry Transposition

instance CacheDefault Transposition CachedTransposition where
  default _ = Transposition 0

instance (MonadState State m, MonadEffect m) => Fetchable m Transposition CachedTransposition where
  fetch _ = liftEffect $ getTranspositionFromUrl

instance (MonadState State m, MonadEffect m) => Flushable m Transposition CachedTransposition where
  flush _ value = liftEffect $ setAppQueryString { transposition: value }

instance Dependable State CachedTransposition where
  dependants _ = fromFoldable [ packPurgeableLens _rewriteResult ]

_transposition :: Lens' State CachedTransposition
_transposition = barlow (key :: _ "!.transposition")


newtype CachedTablatureText = CachedTablatureText (SimpleCacheEntry String)
derive newtype instance CacheEntry String CachedTablatureText
derive newtype instance Purgeable CachedIgnoreDozenalization  

instance CacheDefault String CachedTablatureText where
  default _ = ""

instance (MonadState State m, MonadEffect m) => Fetchable m String CachedTablatureText where
  fetch _ = do
    liftEffect $ getTablatureTextFromUrl <#> Just

instance (MonadState State m, MonadEffect m) => Flushable m String CachedTablatureText where
  flush _ value = liftEffect $ saveTablatureToUrl value

instance Dependable State CachedTablatureText where
  dependants _ = fromFoldable [ packPurgeableLens _parseResult ]

_tablatureText :: Lens' State CachedTablatureText
_tablatureText = barlow (key :: _ "!.tablatureText")


newtype CachedParseResult = CachedParseResult (SimpleCacheEntry TablatureDocument)
derive newtype instance CacheEntry TablatureDocument CachedParseResult 
derive newtype instance Purgeable CachedChordDozenalizationEnabled  

instance CacheDefault TablatureDocument CachedParseResult where
  default _ = Nil

instance (MonadState State m, MonadEffect m) => Fetchable m TablatureDocument CachedParseResult where
  fetch _ = do
    tablatureText <- getM _tablatureText
    pure $ tryParseTablature tablatureText

instance Dependable State CachedParseResult where
  dependants _ = fromFoldable
    [ packPurgeableLens _rewriteResult
    , packPurgeableLens _tablatureTitle ]

_parseResult :: Lens' State CachedParseResult
_parseResult = barlow (key :: _ "!.parseResult")


newtype CachedRewriteResult = CachedRewriteResult (SimpleCacheEntry TablatureDocument)
derive newtype instance CacheEntry TablatureDocument CachedRewriteResult 
derive newtype instance Purgeable CachedTabDozenalizationEnabled  

instance CacheDefault TablatureDocument CachedRewriteResult where
  default _ = Nil

instance (MonadState State m, MonadEffect m) => Fetchable m TablatureDocument CachedRewriteResult where
  fetch _ = do
    parseResult <- getM _parseResult
    renderingOptions <- getRenderingOptions
    pure $ Just $ rewriteTablatureDocument renderingOptions parseResult

instance Dependable State CachedRewriteResult where
  dependants _ = fromFoldable []

_rewriteResult :: Lens' State CachedRewriteResult
_rewriteResult = barlow (key :: _ "!.rewriteResult")


newtype CachedTablatureTitle = CachedTablatureTitle (SimpleCacheEntry String)
derive newtype instance CacheEntry String CachedTablatureTitle 
derive newtype instance Purgeable CachedTabNormalizationEnabled  

instance CacheDefault String CachedTablatureTitle where
  default _ = "Tab viewer"

instance (MonadState State m, MonadEffect m) => Fetchable m String CachedTablatureTitle where
  fetch _ = do
    parseResult <- getM _parseResult
    pure $ getTitle parseResult

instance Dependable State CachedTablatureTitle where
  dependants _ = fromFoldable [ packPurgeableLens _ignoreDozenalization ]

_tablatureTitle :: Lens' State CachedTablatureTitle
_tablatureTitle = barlow (key :: _ "!.tablatureTitle")


newtype CachedTabNormalizationEnabled = CachedTabNormalizationEnabled (SimpleCacheEntry Boolean)
derive newtype instance CacheEntry Boolean CachedTabNormalizationEnabled 
derive newtype instance Purgeable CachedTablatureTitle  

instance CacheDefault Boolean CachedTabNormalizationEnabled where
  default _ = false

instance (MonadState State m, MonadEffect m) => Fetchable m Boolean CachedTabNormalizationEnabled where
  fetch _ = liftEffect $ getLocalStorageBoolean localStorageKeyTabNormalizationEnabled

instance (MonadState State m, MonadEffect m) => Flushable m Boolean CachedTabNormalizationEnabled where
  flush _ value = liftEffect $ setLocalStorageBoolean localStorageKeyTabNormalizationEnabled value

instance Dependable State CachedTabNormalizationEnabled where
  dependants _ = fromFoldable [ packPurgeableLens _rewriteResult ]

_tabNormalizationEnabled :: Lens' State CachedTabNormalizationEnabled
_tabNormalizationEnabled = barlow (key :: _ "!.tabNormalizationEnabled")

localStorageKeyTabNormalizationEnabled :: String
localStorageKeyTabNormalizationEnabled = "tabNormalizationEnabled"


newtype CachedTabDozenalizationEnabled = CachedTabDozenalizationEnabled (SimpleCacheEntry Boolean)
derive newtype instance CacheEntry Boolean CachedTabDozenalizationEnabled 
derive newtype instance Purgeable CachedRewriteResult  

instance CacheDefault Boolean CachedTabDozenalizationEnabled where
  default _ = false

instance (MonadState State m, MonadEffect m) => Fetchable m Boolean CachedTabDozenalizationEnabled where
  fetch _ = liftEffect $ getLocalStorageBoolean localStorageKeyTabDozenalizationEnabled

instance (MonadState State m, MonadEffect m) => Flushable m Boolean CachedTabDozenalizationEnabled where
  flush _ value = liftEffect $ setLocalStorageBoolean localStorageKeyTabDozenalizationEnabled value

instance Dependable State CachedTabDozenalizationEnabled where
  dependants _ = fromFoldable [ packPurgeableLens _rewriteResult ]

_tabDozenalizationEnabled :: Lens' State CachedTabDozenalizationEnabled
_tabDozenalizationEnabled = barlow (key :: _ "!.tabDozenalizationEnabled")

localStorageKeyTabDozenalizationEnabled :: String
localStorageKeyTabDozenalizationEnabled = "tabDozenalizationEnabled"


newtype CachedChordDozenalizationEnabled = CachedChordDozenalizationEnabled (SimpleCacheEntry Boolean)
derive newtype instance CacheEntry Boolean CachedChordDozenalizationEnabled 
derive newtype instance Purgeable CachedParseResult  

instance CacheDefault Boolean CachedChordDozenalizationEnabled where
  default _ = false

instance (MonadState State m, MonadEffect m) => Fetchable m Boolean CachedChordDozenalizationEnabled where
  fetch _ = liftEffect $ getLocalStorageBoolean localStorageKeyChordDozenalizationEnabled

instance (MonadState State m, MonadEffect m) => Flushable m Boolean CachedChordDozenalizationEnabled where
  flush _ value = liftEffect $ setLocalStorageBoolean localStorageKeyChordDozenalizationEnabled value

instance Dependable State CachedChordDozenalizationEnabled where
  dependants _ = fromFoldable [ packPurgeableLens _rewriteResult ]

_chordDozenalizationEnabled :: Lens' State CachedChordDozenalizationEnabled
_chordDozenalizationEnabled = barlow (key :: _ "!.chordDozenalizationEnabled")

localStorageKeyChordDozenalizationEnabled :: String
localStorageKeyChordDozenalizationEnabled = "chordDozenalizationEnabled"


newtype CachedIgnoreDozenalization = CachedIgnoreDozenalization (SimpleCacheEntry Boolean)
derive newtype instance CacheEntry Boolean CachedIgnoreDozenalization 
derive newtype instance Purgeable CachedTablatureText  

instance CacheDefault Boolean CachedIgnoreDozenalization where
  default _ = false

instance (MonadState State m, MonadEffect m) => Fetchable m Boolean CachedIgnoreDozenalization where
  fetch _ = do
    tablatureTitle <- getM _tablatureTitle
    pure $ Just $ test (unsafeRegex "dozenal" ignoreCase) tablatureTitle

instance Dependable State CachedIgnoreDozenalization where
  dependants _ = fromFoldable [ packPurgeableLens _rewriteResult ]

_ignoreDozenalization :: Lens' State CachedIgnoreDozenalization
_ignoreDozenalization = barlow (key :: _ "!.ignoreDozenalization")


-- TODO: move renderingoptions also to cache?
getRenderingOptions :: forall m . MonadState State m => MonadEffect m => m RenderingOptions
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
