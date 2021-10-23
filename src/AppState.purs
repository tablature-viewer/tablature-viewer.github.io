module AppState where

import Data.Lens.Barlow
-- import Data.Lens.Barlow.Helpers
import Prelude

import AppUrl (getTablatureTextFromUrl, saveTablatureToUrl)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Lens (view)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Regex (test)
import Data.String.Regex.Flags (ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect.Class (class MonadEffect)
import Effect.Timer (IntervalId)
import Halogen as H
import TablatureDocument (RenderingOptions, TablatureDocument, Transposition(..), getTitle)
import TablatureParser (parseTablature)
import TablatureRewriter (rewriteTablatureDocument)
import Data.Newtype (class Newtype)

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

data Cache a = NoValue | Cached a

peekOrDefault :: forall a. Cache a -> a -> a
peekOrDefault cache default = case cache of
  NoValue -> default
  Cached result -> result

-- class Cache a where
--   get :: forall output m . MonadEffect m => H.HalogenM State Action () output m a
--   set :: forall output m . MonadEffect m => a -> H.HalogenM State Action () output m Unit
--   peek :: a -> State -> a

class AppStateCache s where
  getTransposition :: forall output m . MonadEffect m => H.HalogenM s Action () output m Transposition
  setTransposition :: forall output m . MonadEffect m => Transposition -> H.HalogenM s Action () output m Unit
  peekTransposition :: s -> Transposition

  getRewriteResult :: forall output m . MonadEffect m => H.HalogenM s Action () output m TablatureDocument
  setRewriteResult :: forall output m . MonadEffect m => TablatureDocument -> H.HalogenM s Action () output m Unit
  peekRewriteResult :: s -> TablatureDocument

  getParseResult :: forall output m . MonadEffect m => H.HalogenM s Action () output m TablatureDocument
  setParseResult :: forall output m . MonadEffect m => TablatureDocument -> H.HalogenM s Action () output m Unit
  peekParseResult :: s -> TablatureDocument

  getTablatureText :: forall output m . MonadEffect m => H.HalogenM s Action () output m String
  setTablatureText :: forall output m . MonadEffect m => String -> H.HalogenM s Action () output m Unit
  peekTablatureText :: s -> String

  getTablatureTitle :: forall output m . MonadEffect m => H.HalogenM s Action () output m String
  peekTablatureTitle :: s -> String

  getTabNormalizationEnabled :: forall output m . MonadEffect m => H.HalogenM s Action () output m Boolean
  peekTabNormalizationEnabled :: s -> Boolean
  toggleTabNormalizationEnabled :: forall output m . MonadEffect m => H.HalogenM s Action () output m Unit
  getTabDozenalizationEnabled :: forall output m . MonadEffect m => H.HalogenM s Action () output m Boolean
  peekTabDozenalizationEnabled :: s -> Boolean
  toggleTabDozenalizationEnabled :: forall output m . MonadEffect m => H.HalogenM s Action () output m Unit
  getChordDozenalizationEnabled :: forall output m . MonadEffect m => H.HalogenM s Action () output m Boolean
  peekChordDozenalizationEnabled :: s -> Boolean
  toggleChordDozenalizationEnabled :: forall output m . MonadEffect m => H.HalogenM s Action () output m Unit

  getAutoscrollTimer :: forall output m . MonadEffect m => H.HalogenM s Action () output m (Maybe IntervalId)
  getAutoscrollSpeed :: forall output m . MonadEffect m => H.HalogenM s Action () output m AutoscrollSpeed
  getAutoscroll :: forall output m . MonadEffect m => H.HalogenM s Action () output m Boolean
  getIgnoreDozenalization :: forall output m . MonadEffect m => H.HalogenM s Action () output m Boolean
  peekIgnoreDozenalization :: s -> Boolean

  -- TODO: move these out of the newtype?
  getScrollTop :: forall output m . MonadEffect m => H.HalogenM s Action () output m Number
  setScrollTop :: forall output m . MonadEffect m => Number -> H.HalogenM s Action () output m Unit
  getLoading :: forall output m . MonadEffect m => H.HalogenM s Action () output m Boolean
  setLoading :: forall output m . MonadEffect m => Boolean -> H.HalogenM s Action () output m Unit
  getMode :: forall output m . MonadEffect m => H.HalogenM s Action () output m Mode
  setMode :: forall output m . MonadEffect m => Mode -> H.HalogenM s Action () output m Unit

  cacheState :: forall output m . MonadEffect m => H.HalogenM s Action () output m Unit


type StateRecord = 
  { mode :: Mode
  , tablatureText :: Cache String
  , tablatureTitle :: Cache String
  , parseResult :: Cache TablatureDocument
  , rewriteResult :: Cache TablatureDocument
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
newtype State = State StateRecord
derive instance newtypeState :: Newtype State _

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
-- Every AppStateCache property should have a dedicated getter and setter function over the halogen monad.
-- The setter should update both the backing storage and purge the affected values in the State.
-- The getter should return from State if present, otherwise fetch from backing store and call the setter.

modify :: forall output m . MonadEffect m => (StateRecord -> StateRecord) -> H.HalogenM State Action () output m Unit
modify f = H.modify_ (\(State s) -> State (f s))

instance appState :: AppStateCache State where
  getTransposition = do
    state <- H.get
    pure $ view (barlow (key :: _ "!.transposition")) state
  setTransposition transposition = modify _ { transposition = transposition, rewriteResult = NoValue }
  peekTransposition state = view (barlow (key :: _ "!.transposition")) state

  getRewriteResult = do
    state <- H.get
    case view (barlow (key :: _ "!.rewriteResult")) state of
      Cached result -> pure result
      NoValue -> do
        parseResult <- getParseResult
        result <- pure $ rewriteTablatureDocument (peekRenderingOptions state) parseResult
        setRewriteResult result
        pure result
  setRewriteResult rewriteResult =
    modify _ { rewriteResult = Cached rewriteResult }
  peekRewriteResult state = peekOrDefault (view (barlow (key :: _ "!.rewriteResult")) state) Nil

  getParseResult = do
    state <- H.get
    case view (barlow (key :: _ "!.parseResult")) state of
      Cached result -> pure result
      NoValue -> do
        tablatureText <- getTablatureText
        result <- pure $ parseTablature tablatureText
        setParseResult result
        pure result
  setParseResult parseResult =
    modify _ { parseResult = Cached parseResult }
  peekParseResult state = peekOrDefault (view (barlow (key :: _ "!.parseResult")) state) Nil

  getTablatureText = do
    state <- H.get
    case view (barlow (key :: _ "!.tablatureText")) state of
      Cached result -> pure result
      NoValue -> do
        tablatureText <- H.liftEffect getTablatureTextFromUrl
        setTablatureText tablatureText
        pure tablatureText
  setTablatureText text = do
    H.liftEffect $ saveTablatureToUrl text
    modify _ { tablatureText = Cached text, parseResult = NoValue, rewriteResult = NoValue, tablatureTitle = NoValue }
  peekTablatureText state = peekOrDefault (view (barlow (key :: _ "!.tablatureText")) state) ""

  getTablatureTitle = do
    state <- H.get
    case view (barlow (key :: _ "!.tablatureTitle")) state of
      Cached result -> pure result
      NoValue -> do
        tablatureDocument <- getRewriteResult
        title <- pure $ fromMaybe defaultTablatureTitle $ getTitle tablatureDocument
        modify _ { tablatureTitle = Cached title }
        pure title
  peekTablatureTitle state = peekOrDefault (view (barlow (key :: _ "!.tablatureTitle")) state) defaultTablatureTitle

  getTabNormalizationEnabled = H.get >>= \state -> pure $ view (barlow (key :: _ "!.tabNormalizationEnabled")) state
  peekTabNormalizationEnabled = view (barlow (key :: _ "!.tabNormalizationEnabled"))
  toggleTabNormalizationEnabled = do
    state <- H.get
    modify _ { tabNormalizationEnabled = not view (barlow (key :: _ "!.tabNormalizationEnabled")) state, rewriteResult = NoValue }

  getTabDozenalizationEnabled = H.get >>= \state -> pure $ view (barlow (key :: _ "!.tabDozenalizationEnabled")) state
  peekTabDozenalizationEnabled = view (barlow (key :: _ "!.tabDozenalizationEnabled"))
  toggleTabDozenalizationEnabled = do
    state <- H.get
    modify _ { tabDozenalizationEnabled = not view (barlow (key :: _ "!.tabDozenalizationEnabled")) state, rewriteResult = NoValue }

  getChordDozenalizationEnabled = H.get >>= \state -> pure $ view (barlow (key :: _ "!.chordDozenalizationEnabled")) state
  peekChordDozenalizationEnabled = view (barlow (key :: _ "!.chordDozenalizationEnabled"))
  toggleChordDozenalizationEnabled = do
    state <- H.get
    modify _ { chordDozenalizationEnabled = not view (barlow (key :: _ "!.chordDozenalizationEnabled")) state, rewriteResult = NoValue }

  getScrollTop = H.get >>= \state -> pure $ view (barlow (key :: _ "!.scrollTop")) state
  setScrollTop x = modify _ { scrollTop = x }
  getLoading = H.get >>= \state -> pure $ view (barlow (key :: _ "!.loading")) state
  setLoading x = modify _ { loading = x }
  getMode = H.get >>= \state -> pure $ view (barlow (key :: _ "!.mode")) state
  setMode x = modify _ { mode = x }

  getAutoscrollTimer = H.get >>= \state -> pure $ view (barlow (key :: _ "!.autoscrollTimer")) state
  getAutoscrollSpeed = H.get >>= \state -> pure $ view (barlow (key :: _ "!.autoscrollSpeed")) state
  getAutoscroll = H.get >>= \state -> pure $ view (barlow (key :: _ "!.autoscroll")) state

  -- TODO also check if tablature contains ↊ or ↋ and cache this value
  getIgnoreDozenalization = do
    title <- getTablatureTitle
    pure $ test (unsafeRegex "dozenal" ignoreCase) title
  peekIgnoreDozenalization = view (barlow (key :: _ "!.ignoreDozenalization"))

  cacheState = do
    _ <- getTablatureTitle
    pure unit

defaultTablatureTitle :: String
defaultTablatureTitle = "Tab viewer"

getRenderingOptions  :: forall output m . MonadEffect m => H.HalogenM State Action () output m RenderingOptions
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

peekRenderingOptions :: State -> RenderingOptions
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


initialState :: forall input. input -> State
initialState _ = State
  { mode: EditMode
  , loading: false
  , tablatureText: Cached ""
  , tablatureTitle: Cached defaultTablatureTitle
  , parseResult: NoValue
  , rewriteResult: NoValue
  , scrollTop: 0.0
  , autoscroll: false
  , autoscrollTimer: Nothing
  , autoscrollSpeed: Normal
  , tabNormalizationEnabled: true
  , tabDozenalizationEnabled: false
  , chordDozenalizationEnabled: false
  , ignoreDozenalization: false
  , transposition: Transposition 0 }
