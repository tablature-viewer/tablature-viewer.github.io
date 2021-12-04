module Main where

import AppActions
import AppHtml
import AppState
import Prelude

import AppUrl (redirectToUrlInFragment)
import AutoscrollSpeed (speedToIntervalMs, speedToIntervalPixelDelta)
import Cache as Cache
import Control.Monad.State (StateT, execStateT)
import Control.Monad.State as MonadState
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Timer (clearInterval, setInterval)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import HalogenUtils (scrollBy)
import LocationString (getQueryParam)
import Web.DOM.Element (scrollTop, setScrollTop)
import Web.HTML (window)
import Web.HTML as WH
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.HTMLElement (focus, toElement)
import Web.HTML.HTMLTextAreaElement as WH.HTMLTextAreaElement
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

type HaloT m a = H.HalogenM State Action () Unit m a

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

handleAction :: forall m . MonadAff m => Action -> HaloT m Unit
handleAction action = do
  prepareHtml action
  liftAff $ delay $ Milliseconds 0.0 -- TODO: this shouldn't be necessary to force rerender
  execAction action
  updateFocus action
  liftAff $ delay $ Milliseconds 0.0 -- TODO: this shouldn't be necessary to force rerender

prepareHtml :: forall m . MonadAff m => Action -> HaloT m Unit
prepareHtml action = do
  setState _loading true
  case action of
    ToggleEditMode -> do
      saveScrollTop
      toggleEditMode
      loadScrollTop
      mode <- viewState _mode
      case mode of
        ViewMode -> focusTablatureContainer
        -- Don't focus the textarea, as the cursor position will be put at the end (which also sometimes makes the window jump)
        EditMode -> pure unit
    _ -> pure unit

execAction :: forall m . MonadAff m => Action -> HaloT m Unit
execAction action = do
  currentState <- MonadState.get
  newState <- execStateT doAction currentState
  H.put newState
  where
  -- We don't do the work directly on the halogen monad, to avoid unnecessary rendering triggers.
  -- We wrap the halogen monad inside another State monad and do the work on the outer state monad.
  doAction :: StateT State (H.HalogenM State Action () Unit m) Unit
  doAction = do
    case action of
      Initialize -> initialize
      ToggleEditMode -> pure unit
      ToggleTabNormalization -> toggleTabNormalization 
      ToggleTabDozenalization -> toggleTabDozenalization 
      ToggleChordDozenalization -> toggleChordDozenalization 
      CreateShortUrl -> createAndCopyShortUrl
      ToggleAutoscroll -> overState _autoscroll not
      IncreaseAutoscrollSpeed -> increaseAutoscrollSpeed
      DecreaseAutoscrollSpeed -> decreaseAutoscrollSpeed
      IncreaseTransposition -> increaseTransposition 
      DecreaseTransposition -> decreaseTransposition 

    lift updateAutoscrollTimer

    tablatureTitle <- Cache.read tablatureTitleCache
    liftEffect $ setDocumentTitle tablatureTitle

    -- TODO: find a generic solution to preload cache
    _ <- Cache.read rewriteResultCache
    setState _loading false

updateFocus :: forall m . MonadAff m => Action -> HaloT m Unit
updateFocus action = 
  case action of
    Initialize -> focusTablatureContainer
    ToggleEditMode -> loadScrollTop
    CreateShortUrl -> focusTablatureContainer
    _ -> pure unit

toggleEditMode :: forall m . MonadEffect m => HaloT m Unit
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

getTablatureEditorElement :: forall m . MonadEffect m => HaloT m (Maybe WH.HTMLTextAreaElement)
getTablatureEditorElement = H.getHTMLElementRef refTablatureEditor <#>
  \maybeHtmlElement -> maybeHtmlElement >>= WH.HTMLTextAreaElement.fromHTMLElement

getTablatureTextFromEditor :: forall m . MonadEffect m => HaloT m String
getTablatureTextFromEditor = do
  maybeTextArea <- getTablatureEditorElement
  case maybeTextArea of
    Nothing -> liftEffect $ Console.error "Could not find textareaTablature" *> pure ""
    Just textArea -> liftEffect $ WH.HTMLTextAreaElement.value textArea 

getTablatureContainerElement :: forall m . MonadEffect m => HaloT m (Maybe WH.HTMLElement)
getTablatureContainerElement = do
  mode <- viewState _mode
  case mode of
    EditMode -> H.getHTMLElementRef refTablatureEditor
    ViewMode -> H.getHTMLElementRef refTablatureViewer

saveScrollTop :: forall m . MonadEffect m => HaloT m Unit
saveScrollTop = do
  maybeTablatureContainerElem <- getTablatureContainerElement <#> \maybeHtmlElement -> maybeHtmlElement <#> toElement
  case maybeTablatureContainerElem of
    Nothing -> liftEffect $ Console.error "Could not find tablatureContainer"
    Just tablatureContainerElem -> do
      newScrollTop <- liftEffect $ scrollTop tablatureContainerElem
      setState _scrollTop newScrollTop

loadScrollTop :: forall m . MonadEffect m => HaloT m Unit
loadScrollTop = do
  maybeTablatureContainerElem <- getTablatureContainerElement <#> \maybeHtmlElement -> maybeHtmlElement <#> toElement
  case maybeTablatureContainerElem of
    Nothing -> liftEffect $ Console.error "Could not find tablatureContainer"
    Just tablatureContainerElem -> do
      scrollTop <- viewState _scrollTop
      liftEffect $ setScrollTop scrollTop tablatureContainerElem

-- TODO: consider treating the js timer as a backing store for a cache?
updateAutoscrollTimer :: forall m . MonadEffect m => HaloT m Unit
updateAutoscrollTimer = do
  autoscrollTimer <- viewState _autoscrollTimer
  autoscroll <- viewState _autoscroll
  case autoscrollTimer of
    Nothing ->
      if autoscroll
      then startAutoscroll
      else pure unit
    Just timerId -> do
      stopAutoscroll timerId -- Always stop and restart, so the speed is up-to-date
      if autoscroll
      then pure unit
      else startAutoscroll
  where
  stopAutoscroll timerId = do
    liftEffect $ clearInterval timerId
    setState _autoscrollTimer Nothing
  startAutoscroll = do
    maybeTablatureContainerElem <- getTablatureContainerElement <#> \maybeHtmlElement -> maybeHtmlElement <#> toElement
    case maybeTablatureContainerElem of
      Nothing -> pure unit
      Just elem -> do
        -- TODO: compensate scrollspeed for fontsize differences between media
        autoscrollSpeed <- viewState _autoscrollSpeed
        timerId <- liftEffect $ setInterval (speedToIntervalMs autoscrollSpeed) $ scrollBy 0 (speedToIntervalPixelDelta autoscrollSpeed) elem
        setState _autoscrollTimer (Just timerId)

focusTablatureContainer :: forall m . MonadEffect m => HaloT m Unit
focusTablatureContainer = do
  maybeTablatureContainerElem <- getTablatureContainerElement
  case maybeTablatureContainerElem of
    Nothing -> liftEffect $ Console.error "Could not find tablatureContainer"
    Just tablatureContainerElem -> liftEffect $ focus tablatureContainerElem


setTablatureTextInEditor :: forall m . MonadEffect m => String -> HaloT m Unit
setTablatureTextInEditor text = do
  maybeTextArea <- getTablatureEditorElement
  case maybeTextArea of
    Nothing -> liftEffect $ Console.error "Could not find textareaTablature" *> pure unit
    Just textArea -> liftEffect $ WH.HTMLTextAreaElement.setValue text textArea 


setDocumentTitle :: String -> Effect Unit
setDocumentTitle title = do
    window <- window
    document <- document window
    setTitle title document
