module Main where

import AppActions
import AppHtml
import AppState
import Prelude

import AppUrl (redirectToUrlInFragment)
import Cache as Cache
import Control.Monad.State (class MonadState, StateT, execStateT)
import Control.Monad.State as MonadState
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import LocationString (getQueryParam)
import TablatureRewriter (NoteOrientation(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document)

foreign import executeJavascriptHacks :: Effect Unit

main :: Effect Unit
main = do
  -- Get query string and maybe redirect to compressed shortlink in the fragment
  mode <- getQueryParam "u"
  case mode of
    Just _ -> redirectToUrlInFragment
    _ -> do
      executeJavascriptHacks
      HA.runHalogenAff do
        body <- HA.awaitBody
        runUI component unit body

component :: forall query input m. MonadAff m => H.Component query input Unit m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

handleAction :: forall m. MonadAff m => Action -> HaloT m Unit
handleAction action = do
  prepareHtml action
  liftAff $ delay $ Milliseconds 0.0 -- TODO: this shouldn't be necessary to force rerender
  currentState <- MonadState.get
  newState <- execStateT (doAction action) currentState
  H.put newState
  updateFocus action
  liftAff $ delay $ Milliseconds 0.0 -- TODO: this shouldn't be necessary to force rerender

prepareHtml :: forall m. MonadAff m => Action -> HaloT m Unit
prepareHtml action = do
  setState _loading true
  case action of
    ToggleEditMode -> do
      saveScrollTop
      toggleEditMode
      mode <- viewState _mode
      case mode of
        ViewMode -> focusTablatureContainer
        -- Don't focus the textarea, as the cursor position will be put at the end (which also sometimes makes the window jump)
        EditMode -> pure unit
        SearchMode -> pure unit
    _ -> pure unit

-- We don't do the work directly on the halogen monad, to avoid unnecessary rendering triggers.
-- We wrap the halogen monad inside another State monad and do the work on the outer state monad.
doAction :: forall m. MonadAff m => Action -> StateT State (H.HalogenM State Action () Unit m) Unit
doAction action = do
  case action of
    Initialize -> initialize
    ToggleEditMode -> pure unit -- Done in prepareHtml
    ToggleTabNormalization -> toggleTabNormalization
    ToggleTabDozenalization -> toggleTabDozenalization
    ToggleChordDozenalization -> toggleChordDozenalization
    ToggleChordNormalization -> toggleChordNormalization
    CreateShortUrl -> createAndCopyShortUrl
    ToggleAutoscroll -> pure unit -- Done in updateAutoscroll
    IncreaseAutoscrollSpeed -> increaseAutoscrollSpeed
    DecreaseAutoscrollSpeed -> decreaseAutoscrollSpeed
    IncreaseTransposition -> increaseTransposition
    DecreaseTransposition -> decreaseTransposition
    FlatNoteOrientation -> setNoteOrientation Flat
    SharpNoteOrientation -> setNoteOrientation Sharp
    DefaultNoteOrientation -> setNoteOrientation Default
    ImportFromUrl url -> importFromUrl url
    SearchInput value -> searchInput value
    ToggleSearch -> do
      toggleSearch
      lift focusSearchInput

  updateAutoscroll action
  updateDocumentTitle

  -- prefetch things that are needed for the rendering
  _ <- Cache.read rewriteResultCache
  setState _loading false

updateDocumentTitle :: forall m. MonadAff m => MonadState State m => m Unit
updateDocumentTitle = do
  tablatureTitle <- Cache.read tablatureTitleCache
  liftEffect $ setDocumentTitle tablatureTitle

updateFocus :: forall m. MonadAff m => Action -> HaloT m Unit
updateFocus action =
  case action of
    Initialize -> focusTablatureContainer
    ToggleEditMode -> loadScrollTop
    CreateShortUrl -> focusTablatureContainer
    _ -> pure unit

toggleEditMode :: forall m. MonadAff m => HaloT m Unit
toggleEditMode = do
  mode <- viewState _mode
  case mode of
    ViewMode -> do
      setState _mode EditMode
      tablatureText <- Cache.read tablatureTextCache
      setTablatureTextInEditor tablatureText
    -- Don't focus the textarea, as the cursor position will be put at the end (which also sometimes makes the window jump)
    EditMode -> do
      tablatureText <- getTablatureTextFromEditor
      Cache.write tablatureTextCache tablatureText
      setState _mode ViewMode
      focusTablatureContainer
    SearchMode -> pure unit

updateAutoscroll :: forall m. MonadAff m => Action -> StateT State (H.HalogenM State Action () Unit m) Unit
updateAutoscroll action = do
  currentAutoscroll <- viewState _autoscroll
  setState _autoscroll $ case action of
    DecreaseAutoscrollSpeed -> true
    IncreaseAutoscrollSpeed -> true
    ToggleAutoscroll -> not currentAutoscroll
    _ -> false
  updateAutoscrollTimer

updateAutoscrollTimer :: forall m. MonadAff m => StateT State (H.HalogenM State Action () Unit m) Unit
updateAutoscrollTimer = do
  stopAutoscroll -- Always stop and restart, so the speed is up-to-date
  autoscroll <- viewState _autoscroll
  if autoscroll then startAutoscroll else pure unit

startAutoscroll :: forall m. MonadAff m => StateT State (H.HalogenM State Action () Unit m) Unit
startAutoscroll = do
  maybeTablatureContainerElem <- lift getTablatureContainerElement
  case maybeTablatureContainerElem of
    Nothing -> pure unit
    Just elem -> startAutoscrollOnElement elem

setDocumentTitle :: String -> Effect Unit
setDocumentTitle title = do
  window <- window
  document <- document window
  setTitle title document
