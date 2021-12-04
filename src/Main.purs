module Main where

import AppActions
import AppState
import Prelude

import AppUrl (redirectToUrlInFragment)
import AutoscrollSpeed (speedToIntervalMs, speedToIntervalPixelDelta)
import Cache as Cache
import Control.Monad.State (class MonadState, StateT, execStateT, state)
import Control.Monad.State as MonadState
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Array (fromFoldable)
import Data.Lens.Barlow (key)
import Data.Lens.Barlow.Helpers (view)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import DebugUtils (debug)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Timer (clearInterval, setInterval)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import HalogenUtils (classString, fontAwesome, optionalText, scrollBy)
import LocationString (getQueryParam)
import TablatureRenderer (renderTablatureDocument)
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

refTablatureEditor :: H.RefLabel
refTablatureEditor = H.RefLabel "tablatureEditor"

refTablatureViewer :: H.RefLabel
refTablatureViewer = H.RefLabel "tablatureViewer"

type HaloT m a = H.HalogenM State Action () Unit m a

component :: forall query input m. MonadAff m => H.Component query input Unit m
component =
  H.mkComponent
    { initialState
    , render
    , eval: eval
    }

eval :: forall m query input a. MonadAff m => H.HalogenQ query Action input a -> HaloT m a
eval = H.mkEval H.defaultEval 
      { handleAction = handleAction 
      , initialize = Just Initialize
      }

render :: forall m. State -> H.ComponentHTML Action () m
render state = debug "rendering" $ HH.div_
  [ HH.div 
    [ classString "app" ]
    [ renderHeader
    , renderBody
    ]
  , HH.div
    [ classString "tablaturePrinter tablature" ]
    [ HH.pre_ $ renderTablature state ]
  ]
  where
  renderBody = case view (key :: _ "!.mode") state of
    ViewMode -> HH.div 
      [ classString "tablatureViewer tablature"
      , HP.ref refTablatureViewer
      , HP.tabIndex 0
      ]
      [ HH.pre_ $ renderTablature state ]
    EditMode -> HH.textarea
      [ HP.ref refTablatureEditor
      , classString "tablatureEditor" 
      , HP.placeholder "Paste your plaintext tablature or chord sheet here, click 'Save' and bookmark it"
      , HP.spellcheck false
      ]
  renderHeader = HH.div
    [ classString "header" ]
    [ renderTitle
    , renderControls
    , renderLoadingIcon
    ]
  renderTitle = HH.div
    [ classString "title largeViewport"]
    [ HH.a
      [ HP.href "https://github.com/tablature-viewer/tablature-viewer.github.io"
      , HP.target "_blank"
      ]
      [ HH.h1_ [ HH.text "Tablature Viewer" ] ]
    ]
  renderLoadingIcon = HH.div 
    [ classString "loadingIcon lds-ellipsis" ]
    if view (key :: _ "!.loading") state then [ HH.div_ [], HH.div_ [], HH.div_ [], HH.div_ [] ] else []
  renderControls = HH.div 
    [ classString "controls" ]
    [ HH.div
      [ classString "dropdown-container" ]
      [ HH.button
        [ HP.title "Settings"
        , classString "dropdown-header" ]
        [ fontAwesome "fa-wrench"
        , optionalText " Settings"
        ]
      , HH.div [ classString "dropdown-menu" ]
        [ HH.button
          [ HP.title "Toggle normalization for tabs on or off"
          , classString "dropdown-item"
          , HE.onClick \_ -> ToggleTabNormalization
          ]
          [ if Cache.peek _tabNormalizationEnabled state
              then fontAwesome "fa-toggle-on"
              else fontAwesome "fa-toggle-off"
          , HH.text " Normalize tabs"
          ]
        , HH.button
          [ HP.title "Toggle decimal to dozenal conversion for tabs on or off"
          , classString "dropdown-item"
          , HE.onClick \_ -> ToggleTabDozenalization
          , classString $ if Cache.peek _ignoreDozenalization state then "disabled" else ""
          ]
          [ if Cache.peek _tabDozenalizationEnabled state && not Cache.peek _ignoreDozenalization state
              then fontAwesome "fa-toggle-on"
              else fontAwesome "fa-toggle-off"
          , HH.text " Dozenalize tabs"
          ]
        , HH.button
          [ HP.title "Toggle decimal to dozenal conversion for chords on or off"
          , HE.onClick \_ -> ToggleChordDozenalization
          , classString "dropdown-item"
          , classString $ if Cache.peek _ignoreDozenalization state then "disabled" else ""
          ]
          [ if Cache.peek _chordDozenalizationEnabled state && not Cache.peek _ignoreDozenalization state
              then fontAwesome "fa-toggle-on"
              else fontAwesome "fa-toggle-off"
          , HH.text " Dozenalize chords"
          ]
        , HH.div
          [ HP.title "Transpose the tablature"
          , classString "dropdown-item"
          ]
          [ HH.button
            [ HP.title "Transpose down"
            , HE.onClick \_ -> DecreaseTransposition
            ]
            [ fontAwesome "fa-caret-down" ]
          , HH.button
            [ HP.title "Transpose up"
            , HE.onClick \_ -> IncreaseTransposition
            ]
            [ fontAwesome "fa-caret-up" ]
          , HH.span_ [ HH.text $ " Transpose " <> show (Cache.peek _transposition state) ]
          ]
        , HH.div
          [ HP.title "Change the autoscroll speed"
          , classString "dropdown-item"
          ]
          [ HH.button
            [ HP.title "Decrease the autoscroll speed"
            , HE.onClick \_ -> DecreaseAutoscrollSpeed
            ]
            [ fontAwesome "fa-backward" ]
          , HH.button
            [ HP.title "Increase the autoscroll speed"
            , HE.onClick \_ -> IncreaseAutoscrollSpeed
            ]
            [ fontAwesome "fa-forward" ]
          , HH.span_ [ HH.text $ " Autoscroll speed " <> show (view (key :: _ "!.autoscrollSpeed") state) ]
          ]
        ]
      ]
    , HH.a
      [ HP.href "https://github.com/tablature-viewer/tablature-viewer.github.io"
      , HP.target "_blank"
      , HP.tabIndex (-1)
      ]
      [ HH.button [ HP.title "Open the README in a new browser tab" ] [ fontAwesome "fa-question", optionalText " Readme" ] ]
    , HH.button [ HP.title toggleButtonTitle, HE.onClick \_ -> ToggleEditMode ] toggleButtonContent
    , HH.a
      [ HP.href "./"
      , HP.target "_blank"
      , HP.tabIndex (-1)
      ]
      [ HH.button
        [ HP.title "Open an empty tablature in a new browser tab" ] [ fontAwesome "fa-plus", optionalText " New" ] ]
    , HH.button
      [ HP.title "Create a short link to the tablature for sharing with other people"
      , HE.onClick \_ -> CopyShortUrl
      ] [ fontAwesome "fa-share", optionalText " Share" ]
    , HH.button
      [ HP.title "Toggle autoscrolling"
      , HE.onClick \_ -> ToggleAutoscroll
      ] toggleAutoscrollContent
    ]
    where
    toggleButtonContent = case view (key :: _ "!.mode") state of
      EditMode -> [ fontAwesome "fa-save", optionalText " Save" ]
      ViewMode  -> [ fontAwesome "fa-edit", optionalText " Edit" ]
    toggleButtonTitle = case view (key :: _ "!.mode") state of
      EditMode -> "Save tablature"
      ViewMode  -> "Edit tablature"
    toggleAutoscrollContent =
      if view (key :: _ "!.autoscroll") state
      then [ fontAwesome "fa-stop", optionalText " Autoscroll" ]
      else [ fontAwesome "fa-play", optionalText " Autoscroll" ]


renderTablature :: forall w i . State -> Array (HH.HTML w i)
renderTablature state = fromFoldable $ renderTablatureDocument (Cache.peek _rewriteResult state)

handleAction :: forall m . MonadAff m => Action -> HaloT m Unit
handleAction action = do
  modifyState _ { loading = true }
  liftAff $ delay $ Milliseconds 0.0 -- TODO: this shouldn't be necessary to force rerender

  -- We don't do the work directly on the halogen monad, to avoid unnecessary rendering triggers.
  -- We wrap the halogen monad inside another State monad and do the work on the outer state monad.
  currentState <- MonadState.get
  newState <- execStateT (doAction action) currentState
  -- Put the state of the outer state monad in the halogen monad
  H.put $ newState

  updateAutoscrollTimer

  modifyState _ { loading = false }

doAction :: forall m . MonadAff m => Action -> StateT State (H.HalogenM State Action () Unit m) Unit
doAction action = do
  state <- getState
  case action of
    Initialize -> do
      initialize
      lift focusTablatureContainer
    ToggleEditMode -> do
      lift saveScrollTop
      toggleEditMode
      lift loadScrollTop
    ToggleTabNormalization -> toggleTabNormalization 
    ToggleTabDozenalization -> toggleTabDozenalization 
    ToggleChordDozenalization -> toggleChordDozenalization 
    CopyShortUrl -> do
      copyShortUrl
      lift focusTablatureContainer
    ToggleAutoscroll -> modifyState _ { autoscroll = not state.autoscroll }
    IncreaseAutoscrollSpeed -> increaseAutoscrollSpeed
    DecreaseAutoscrollSpeed -> decreaseAutoscrollSpeed
    IncreaseTransposition -> increaseTransposition 
    DecreaseTransposition -> decreaseTransposition 

  lift updateAutoscrollTimer

  tablatureTitle <- Cache.read cachedTablatureTitle
  liftEffect $ setDocumentTitle tablatureTitle

  -- TODO: find a generic solution to preload cache
  _ <- Cache.read cachedRewriteResult
  pure unit

getTablatureEditorElement :: forall m . MonadEffect m => HaloT m (Maybe WH.HTMLTextAreaElement)
getTablatureEditorElement = H.getHTMLElementRef refTablatureEditor <#>
  \maybeHtmlElement -> maybeHtmlElement >>= WH.HTMLTextAreaElement.fromHTMLElement

getTablatureTextFromEditor :: forall m  . MonadEffect m => HaloT m String
getTablatureTextFromEditor = do
  maybeTextArea <- getTablatureEditorElement
  case maybeTextArea of
    Nothing -> liftEffect $ Console.error "Could not find textareaTablature" *> pure ""
    Just textArea -> liftEffect $ WH.HTMLTextAreaElement.value textArea 

getTablatureContainerElement :: forall m . MonadEffect m => HaloT m (Maybe WH.HTMLElement)
getTablatureContainerElement = do
  state <- getState
  case state.mode of
    EditMode -> H.getHTMLElementRef refTablatureEditor
    ViewMode -> H.getHTMLElementRef refTablatureViewer

saveScrollTop :: forall m  . MonadEffect m => HaloT m Unit
saveScrollTop = do
  maybeTablatureContainerElem <- getTablatureContainerElement <#> \maybeHtmlElement -> maybeHtmlElement <#> toElement
  case maybeTablatureContainerElem of
    Nothing -> liftEffect $ Console.error "Could not find tablatureContainer"
    Just tablatureContainerElem -> do
      newScrollTop <- liftEffect $ scrollTop tablatureContainerElem
      modifyState _ { scrollTop = newScrollTop }
      pure unit

loadScrollTop :: forall m  . MonadEffect m => HaloT m Unit
loadScrollTop = do
  state <- getState
  maybeTablatureContainerElem <- getTablatureContainerElement <#> \maybeHtmlElement -> maybeHtmlElement <#> toElement
  case maybeTablatureContainerElem of
    Nothing -> liftEffect $ Console.error "Could not find tablatureContainer"
    Just tablatureContainerElem -> do
      liftEffect $ setScrollTop state.scrollTop tablatureContainerElem

-- TODO: consider treating the js timer as a backing store for a cache?
updateAutoscrollTimer :: forall m  . MonadEffect m => HaloT m Unit
updateAutoscrollTimer = do
  -- state <- getState
  State state <- MonadState.get
  case state.autoscrollTimer of
    Nothing ->
      if state.autoscroll
      then startAutoscroll
      else pure unit
    Just timerId -> do
      stopAutoscroll timerId -- Always stop and restart, so the speed is up-to-date
      if state.autoscroll
      then pure unit
      else startAutoscroll
  where
  stopAutoscroll timerId = do
    liftEffect $ clearInterval timerId
    modifyState _ { autoscrollTimer = Nothing }
  startAutoscroll = do
    state <- getState
    maybeTablatureContainerElem <- getTablatureContainerElement <#> \maybeHtmlElement -> maybeHtmlElement <#> toElement
    case maybeTablatureContainerElem of
      Nothing -> pure unit
      Just elem -> do
        -- TODO: compensate scrollspeed for fontsize differences between media
        timerId <- liftEffect $ setInterval (speedToIntervalMs state.autoscrollSpeed) $ scrollBy 0 (speedToIntervalPixelDelta state.autoscrollSpeed) elem
        modifyState _ { autoscrollTimer = Just timerId }

focusTablatureContainer :: forall m  . MonadEffect m => HaloT m Unit
focusTablatureContainer = do
  maybeTablatureContainerElem <- getTablatureContainerElement
  case maybeTablatureContainerElem of
    Nothing -> liftEffect $ Console.error "Could not find tablatureContainer"
    Just tablatureContainerElem -> liftEffect $ focus tablatureContainerElem


setTablatureTextInEditor :: forall m  . MonadEffect m => String -> HaloT m Unit
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
