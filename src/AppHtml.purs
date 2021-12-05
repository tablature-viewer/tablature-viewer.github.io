module AppHtml where

import AppActions
import AppState
import Prelude

import AutoscrollSpeed (speedToIntervalMs, speedToIntervalPixelDelta)
import Cache as Cache
import Control.Monad.State (class MonadState)
import Data.Array (fromFoldable)
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Timer (clearInterval, setInterval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenUtils (classString, fontAwesome, optionalText, scrollBy)
import TablatureRenderer (renderTablatureDocument)
import Web.DOM (Element)
import Web.DOM.Element (scrollTop, setScrollTop)
import Web.HTML as WH
import Web.HTML.HTMLElement (focus, toElement)
import Web.HTML.HTMLTextAreaElement as WH.HTMLTextAreaElement


type HaloT m a = H.HalogenM State Action () Unit m a

refTablatureEditor :: H.RefLabel
refTablatureEditor = H.RefLabel "tablatureEditor"

refTablatureViewer :: H.RefLabel
refTablatureViewer = H.RefLabel "tablatureViewer"

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div_
  [ HH.div 
    [ classString "app" ]
    [ renderHeader
    , renderBody
    ]
  , HH.div
    [ classString "tablaturePrinter tablature" ]
    [ HH.pre_ $ renderTablature ]
  ]
  where
  renderTablature = fromFoldable $ renderTablatureDocument (Cache.peek _rewriteResult state)
  renderBody = case view _mode state of
    ViewMode -> HH.div 
      [ classString "tablatureViewer tablature"
      , HP.ref refTablatureViewer
      , HP.tabIndex 0
      ]
      [ HH.pre_ $ renderTablature ]
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
    if view _loading state then [ HH.div_ [], HH.div_ [], HH.div_ [], HH.div_ [] ] else []
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
        , if Cache.peek _ignoreDozenalization state
          then HH.button
            [ HP.title "Tablature is already dozenal"
            , classString "dropdown-item disabled"
            ]
            [ fontAwesome "fa-toggle-off"
            , HH.text " Dozenalize tabs"
            ]
          else HH.button
            [ HP.title "Toggle decimal to dozenal conversion for tabs on or off"
            , classString "dropdown-item"
            , HE.onClick \_ -> ToggleTabDozenalization
            ]
            [ if Cache.peek _tabDozenalizationEnabled state
              then fontAwesome "fa-toggle-on"
              else fontAwesome "fa-toggle-off"
            , HH.text " Dozenalize tabs"
            ]
        , if Cache.peek _ignoreDozenalization state
          then HH.button
            [ HP.title "Tablature is already dozenal"
            , classString "dropdown-item disabled"
            ]
            [ fontAwesome "fa-toggle-off"
            , HH.text " Dozenalize chords"
            ]
          else HH.button
            [ HP.title "Toggle decimal to dozenal conversion for chords on or off"
            , HE.onClick \_ -> ToggleChordDozenalization
            , classString "dropdown-item"
            ]
            [ if Cache.peek _chordDozenalizationEnabled state
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
          , HH.span_ [ HH.text $ " Autoscroll speed " <> show (view _autoscrollSpeed state) ]
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
      , HE.onClick \_ -> CreateShortUrl
      ] [ fontAwesome "fa-share", optionalText " Share" ]
    , HH.button
      [ HP.title "Toggle autoscrolling"
      , HE.onClick \_ -> ToggleAutoscroll
      ] toggleAutoscrollContent
    ]
    where
    toggleButtonContent = case view _mode state of
      EditMode -> [ fontAwesome "fa-save", optionalText " Save" ]
      ViewMode -> [ fontAwesome "fa-edit", optionalText " Edit" ]
    toggleButtonTitle = case view _mode state of
      EditMode -> "Save tablature"
      ViewMode -> "Edit tablature"
    toggleAutoscrollContent =
      if view _autoscroll state
      then [ fontAwesome "fa-stop", optionalText " Autoscroll" ]
      else [ fontAwesome "fa-play", optionalText " Autoscroll" ]


getTablatureEditorElement :: forall m . MonadEffect m => HaloT m (Maybe WH.HTMLTextAreaElement)
getTablatureEditorElement = H.getHTMLElementRef refTablatureEditor <#>
  \maybeHtmlElement -> maybeHtmlElement >>= WH.HTMLTextAreaElement.fromHTMLElement

getTablatureTextFromEditor :: forall m . MonadEffect m => HaloT m String
getTablatureTextFromEditor = do
  maybeTextArea <- getTablatureEditorElement
  case maybeTextArea of
    Nothing -> liftEffect $ Console.error "Could not find textareaTablature" *> pure ""
    Just textArea -> liftEffect $ WH.HTMLTextAreaElement.value textArea 

getTablatureContainerHtmlElement :: forall m . MonadEffect m => HaloT m (Maybe WH.HTMLElement)
getTablatureContainerHtmlElement = do
  mode <- viewState _mode
  case mode of
    EditMode -> H.getHTMLElementRef refTablatureEditor
    ViewMode -> H.getHTMLElementRef refTablatureViewer

getTablatureContainerElement :: forall m . MonadEffect m => HaloT m (Maybe Element)
getTablatureContainerElement = getTablatureContainerHtmlElement <#> \maybeHtmlElement -> maybeHtmlElement <#> toElement

saveScrollTop :: forall m . MonadEffect m => HaloT m Unit
saveScrollTop = do
  maybeTablatureContainerElem <- getTablatureContainerHtmlElement <#> \maybeHtmlElement -> maybeHtmlElement <#> toElement
  case maybeTablatureContainerElem of
    Nothing -> liftEffect $ Console.error "Could not find tablatureContainer"
    Just tablatureContainerElem -> do
      newScrollTop <- liftEffect $ scrollTop tablatureContainerElem
      setState _scrollTop newScrollTop

loadScrollTop :: forall m . MonadEffect m => HaloT m Unit
loadScrollTop = do
  maybeTablatureContainerElem <- getTablatureContainerHtmlElement <#> \maybeHtmlElement -> maybeHtmlElement <#> toElement
  case maybeTablatureContainerElem of
    Nothing -> liftEffect $ Console.error "Could not find tablatureContainer"
    Just tablatureContainerElem -> do
      scrollTop <- viewState _scrollTop
      liftEffect $ setScrollTop scrollTop tablatureContainerElem


focusTablatureContainer :: forall m . MonadEffect m => HaloT m Unit
focusTablatureContainer = do
  maybeTablatureContainerElem <- getTablatureContainerHtmlElement
  case maybeTablatureContainerElem of
    Nothing -> liftEffect $ Console.error "Could not find tablatureContainer"
    Just tablatureContainerElem -> liftEffect $ focus tablatureContainerElem


setTablatureTextInEditor :: forall m . MonadEffect m => String -> HaloT m Unit
setTablatureTextInEditor text = do
  maybeTextArea <- getTablatureEditorElement
  case maybeTextArea of
    Nothing -> liftEffect $ Console.error "Could not find textareaTablature" *> pure unit
    Just textArea -> liftEffect $ WH.HTMLTextAreaElement.setValue text textArea 

stopAutoscroll :: forall m . MonadEffect m => MonadState State m => m Unit
stopAutoscroll = do
  autoscrollTimer <- viewState _autoscrollTimer
  case autoscrollTimer of
    Nothing -> pure unit
    Just intervalId -> do
      liftEffect $ clearInterval intervalId
      setState _autoscrollTimer Nothing

startAutoscrollOnElement :: forall m . MonadEffect m => MonadState State m => Element -> m Unit
startAutoscrollOnElement elem = do
  autoscrollSpeed <- viewState _autoscrollSpeed
  intervalId <- liftEffect $ setInterval (speedToIntervalMs autoscrollSpeed) $ scrollBy 0 (speedToIntervalPixelDelta autoscrollSpeed) elem
  setState _autoscrollTimer (Just intervalId)
