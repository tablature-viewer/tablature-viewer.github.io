module AppState where

import Prelude

import AppUrl (getTablatureTextFromUrl, saveTablatureToUrl)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect)
import Effect.Timer (IntervalId)
import Halogen as H
import TablatureDocument (RenderingOptions, TablatureDocument, Transposition, getTitle)
import TablatureParser (parseTablature)
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

data Cached a = NoValue | Cached a

class Cache a where
  get :: forall output m . MonadEffect m => H.HalogenM State Action () output m a
  set :: forall output m . MonadEffect m => a -> H.HalogenM State Action () output m Unit
  peek :: a -> State -> a

-- TODO: rename to AppState?
type State =
  { mode :: Mode
  , tablatureText :: Cached String
  , tablatureTitle :: Cached String
  , parseResult :: Cached TablatureDocument
  , rewriteResult :: Cached TablatureDocument
  , tabNormalizationEnabled :: Boolean
  , tabDozenalizationEnabled :: Boolean
  , chordDozenalizationEnabled :: Boolean
  , transposition :: Transposition
  , autoscrollTimer :: Maybe IntervalId
  , autoscrollSpeed :: AutoscrollSpeed
  , autoscroll :: Boolean
  -- For tabs that are already dozenal themselves we want to ignore any dozenalization settings
  , ignoreDozenalization :: Boolean
  -- Store the scrollTop in the state before actions so we can restore the expected scrollTop when switching views
  , scrollTop :: Number
  , loading :: Boolean
  }

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


-- TODO
-- Every AppState property should have a dedicated getter and setter function over the halogen monad.
-- The setter should update both the backing storage and purge the affected values in the State.
-- The getter should return from State if present, otherwise fetch from backing store and call the setter.
getTransposition :: forall output m . MonadEffect m => H.HalogenM State Action () output m Transposition
getTransposition = do
  state <- H.get
  pure state.transposition
setTransposition :: forall output m . MonadEffect m => Transposition -> H.HalogenM State Action () output m Unit
setTransposition transposition =
  H.modify_ _ { transposition = transposition, rewriteResult = NoValue }

instance transpositionCache :: Cache Transposition where
  get = getTransposition
  set = setTransposition
  peek _ state = state.transposition

getRewriteResult :: forall output m . MonadEffect m => H.HalogenM State Action () output m TablatureDocument
getRewriteResult = do
  state <- H.get
  case state.rewriteResult of
    Cached result -> pure result
    NoValue -> do
      parseResult <- getParseResult
      result <- pure $ rewriteTablatureDocument (getRenderingOptions state) parseResult
      setRewriteResult result
      pure result
setRewriteResult :: forall output m . MonadEffect m => TablatureDocument -> H.HalogenM State Action () output m Unit
setRewriteResult rewriteResult =
  H.modify_ _ { rewriteResult = Cached rewriteResult }

getParseResult :: forall output m . MonadEffect m => H.HalogenM State Action () output m TablatureDocument
getParseResult = do
  state <- H.get
  case state.parseResult of
    Cached result -> pure result
    NoValue -> do
      tablatureText <- getTablatureText
      result <- pure $ parseTablature tablatureText
      setParseResult result
      pure result
setParseResult :: forall output m . MonadEffect m => TablatureDocument -> H.HalogenM State Action () output m Unit
setParseResult parseResult =
  H.modify_ _ { parseResult = Cached parseResult }

getTablatureText :: forall output m . MonadEffect m => H.HalogenM State Action () output m String
getTablatureText = do
  state <- H.get
  case state.tablatureText of
    Cached result -> pure result
    NoValue -> do
      tablatureText <- H.liftEffect getTablatureTextFromUrl
      setTablatureText tablatureText
      pure tablatureText
setTablatureText :: forall output m . MonadEffect m => String -> H.HalogenM State Action () output m Unit
setTablatureText text = do
  H.liftEffect $ saveTablatureToUrl text
  H.modify_ _ { tablatureText = Cached text, parseResult = NoValue, rewriteResult = NoValue, tablatureTitle = NoValue }

getTablatureTitle :: forall output m . MonadEffect m => H.HalogenM State Action () output m String
getTablatureTitle = do
  state <- H.get
  case state.tablatureTitle of
    Cached result -> pure result
    NoValue -> do
      tablatureDocument <- getRewriteResult
      title <- pure $ getTitle tablatureDocument
      H.modify_ _ { tablatureTitle = Cached title }
      pure title


cacheState :: forall output m . MonadEffect m => H.HalogenM State Action () output m Unit
cacheState = do
  _ <- getTablatureTitle
  pure unit

getRenderingOptions :: State -> RenderingOptions
getRenderingOptions state =
  { dozenalizeTabs: state.tabDozenalizationEnabled && not state.ignoreDozenalization
  , dozenalizeChords: state.chordDozenalizationEnabled && not state.ignoreDozenalization
  , normalizeTabs: state.tabNormalizationEnabled
  , transposition: state.transposition }
