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
import TablatureRewriter (NoteOrientation(..))
import Web.DOM (Element)
import Web.DOM.Element (scrollTop, setScrollTop)
import Web.HTML as WH
import Web.HTML.HTMLElement (focus, toElement)
import Web.HTML.HTMLInputElement as WH.HTMLInputElement
import Web.HTML.HTMLTextAreaElement as WH.HTMLTextAreaElement

type HaloT m a = H.HalogenM State Action () Unit m a

refTablatureEditor :: H.RefLabel
refTablatureEditor = H.RefLabel "tablatureEditor"

refTablatureViewer :: H.RefLabel
refTablatureViewer = H.RefLabel "tablatureViewer"

refTablatureSearchInput :: H.RefLabel
refTablatureSearchInput = H.RefLabel "tablatureSearchInput"

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
    SearchMode -> HH.div
      [ classString "tablatureSearch"
      -- , HP.tabIndex 0
      ]
      [ renderSearchBar, renderSearchResults ]
  renderSearchBar =
    HH.input
      [ HP.ref refTablatureSearchInput
      , HE.onKeyDown \event -> SearchInput event
      ]
  renderSearchResults = HH.div []
    [ case view _searchResults state of
        Nothing -> HH.span [] [ HH.text "Type something in the search bar" ]
        Just [] -> HH.span [] [ HH.text "No search results" ]
        Just searchResults -> HH.table [] (searchResults <#> renderSearchResult)
    ]
  renderSearchResult searchResult = HH.tr
    [ HE.onClick \_ -> ImportFromUrl searchResult.url
    ]
    [ HH.td [] [ HH.text searchResult.name ]
    , HH.td [] [ HH.text $ show searchResult.rating ]
    , HH.td [] [ HH.text $ show searchResult.contentType ]
    ]
  renderHeader = HH.div
    [ classString "header" ]
    [ renderTitle
    , renderControls
    , renderLoadingIcon
    ]
  renderTitle = HH.div
    [ classString "title largeViewport" ]
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
            , classString "header-button dropdown-header"
            ]
            [ fontAwesome "fa-wrench"
            , optionalText " Settings"
            ]
        , renderSettingsMenu
        ]
    , HH.a
        [ HP.href "https://github.com/tablature-viewer/tablature-viewer.github.io"
        , HP.target "_blank"
        , HP.tabIndex (-1)
        , classString "header-button"
        ]
        [ HH.button [ HP.title "Open the README in a new browser tab" ] [ fontAwesome "fa-question", optionalText " Readme" ] ]
    , HH.button
        [ HP.title toggleButtonTitle
        , HE.onClick \_ -> ToggleEditMode
        , classString "header-button"
        ]
        toggleButtonContent
    , HH.a
        [ HP.href "./"
        , HP.target "_blank"
        , HP.tabIndex (-1)
        , classString "header-button"
        ]
        [ HH.button
            [ HP.title "Open an empty tablature in a new browser tab" ]
            [ fontAwesome "fa-plus", optionalText " New" ]
        ]
    , HH.button
        [ HP.title "Create a short link to the tablature for sharing with other people"
        , HE.onClick \_ -> CreateShortUrl
        , classString "header-button"
        ]
        [ fontAwesome "fa-share", optionalText " Share" ]
    , HH.button
        [ HP.title "Toggle autoscrolling"
        , HE.onClick \_ -> ToggleAutoscroll
        , classString "header-button"
        ]
        toggleAutoscrollContent
    , HH.button
        [ HP.title "Search tab"
        , HE.onClick \_ -> OpenSearch
        , classString "header-button"
        ]
        [ fontAwesome "fa-search", optionalText " Search" ]
    ]
  toggleButtonContent = case view _mode state of
    EditMode -> [ fontAwesome "fa-save", optionalText " Save" ]
    _ -> [ fontAwesome "fa-edit", optionalText " Edit" ]
  toggleButtonTitle = case view _mode state of
    EditMode -> "Save tablature"
    _ -> "Edit tablature"
  toggleAutoscrollContent =
    if view _autoscroll state then [ fontAwesome "fa-stop", optionalText " Autoscroll" ]
    else [ fontAwesome "fa-play", optionalText " Autoscroll" ]
  renderSettingsMenu =
    HH.div [ classString "dropdown-menu" ]
      [ HH.div
          [ classString "dropdown-item" ]
          [ HH.button
              [ HP.title "Toggle normalization for tabs on or off"
              , HE.onClick \_ -> ToggleTabNormalization
              ]
              [ if Cache.peek _tabNormalizationEnabled state then fontAwesome "fa-toggle-on"
                else fontAwesome "fa-toggle-off"
              ]
          , HH.div_ [ HH.text $ "Normalize tabs" ]
          ]
      , HH.div
          [ classString "dropdown-item" ]
          [ if Cache.peek _ignoreDozenalization state then HH.button
              [ HP.title "Tablature is already dozenal"
              , classString "disabled"
              ]
              [ fontAwesome "fa-toggle-off" ]
            else HH.button
              [ HP.title "Toggle decimal to dozenal conversion for tabs on or off"
              , HE.onClick \_ -> ToggleTabDozenalization
              ]
              [ if Cache.peek _tabDozenalizationEnabled state then fontAwesome "fa-toggle-on"
                else fontAwesome "fa-toggle-off"
              ]
          , HH.div_ [ HH.text $ "Dozenalize tabs" ]
          ]
      , HH.div
          [ classString "dropdown-item" ]
          [ HH.button
              [ HP.title "Toggle normalization for chords on or off"
              , HE.onClick \_ -> ToggleChordNormalization
              ]
              [ if Cache.peek _chordNormalizationEnabled state then fontAwesome "fa-toggle-on"
                else fontAwesome "fa-toggle-off"
              ]
          , HH.div_ [ HH.text $ "Normalize chords" ]
          ]
      , HH.div
          [ classString "dropdown-item" ]
          [ if Cache.peek _ignoreDozenalization state then HH.button
              [ HP.title "Tablature is already dozenal"
              , classString "disabled"
              ]
              [ fontAwesome "fa-toggle-off" ]
            else HH.button
              [ HP.title "Toggle decimal to dozenal conversion for chords on or off"
              , HE.onClick \_ -> ToggleChordDozenalization
              ]
              [ if Cache.peek _chordDozenalizationEnabled state then fontAwesome "fa-toggle-on"
                else fontAwesome "fa-toggle-off"
              ]
          , HH.div_ [ HH.text $ " Dozenalize chords" ]
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
          , HH.div_ [ HH.text $ "Transpose " <> show (Cache.peek _urlParams state).transposition ]
          ]
      , HH.div
          [ HP.title "Preferred note orientation"
          , classString "dropdown-item"
          ]
          let
            noteOrientation = (Cache.peek _urlParams state).noteOrientation
          in
            [ HH.button
                [ HP.title "default"
                , HE.onClick \_ -> DefaultNoteOrientation
                , if noteOrientation == Default then classString "selected" else classString ""
                ]
                [ HH.text "default"
                ]
            , HH.button
                [ HP.title "Prefer flat"
                , HE.onClick \_ -> FlatNoteOrientation
                , if noteOrientation == Flat then classString "selected" else classString ""
                ]
                [ HH.text "flat"
                ]
            , HH.button
                [ HP.title "Prefer sharp"
                , HE.onClick \_ -> SharpNoteOrientation
                , if noteOrientation == Sharp then classString "selected" else classString ""
                ]
                [ HH.text "sharp"
                ]
            , HH.div_ [ HH.text $ "Note orientation" ]
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
          , HH.div_ [ HH.text $ "Autoscroll speed " <> show (view _autoscrollSpeed state) ]
          ]
      ]

getSearchInputHtmlElement :: forall m. MonadEffect m => HaloT m (Maybe WH.HTMLElement)
getSearchInputHtmlElement = H.getHTMLElementRef refTablatureSearchInput

getSearchInputElement :: forall m. MonadEffect m => HaloT m (Maybe WH.HTMLInputElement)
getSearchInputElement = getSearchInputHtmlElement <#>
  \maybeHtmlElement -> maybeHtmlElement >>= WH.HTMLInputElement.fromHTMLElement

getTablatureEditorElement :: forall m. MonadEffect m => HaloT m (Maybe WH.HTMLTextAreaElement)
getTablatureEditorElement = H.getHTMLElementRef refTablatureEditor <#>
  \maybeHtmlElement -> maybeHtmlElement >>= WH.HTMLTextAreaElement.fromHTMLElement

getTablatureTextFromEditor :: forall m. MonadEffect m => HaloT m String
getTablatureTextFromEditor = do
  maybeTextArea <- getTablatureEditorElement
  case maybeTextArea of
    Nothing -> liftEffect $ Console.error "Could not find textareaTablature" *> pure ""
    Just textArea -> liftEffect $ WH.HTMLTextAreaElement.value textArea

getTablatureContainerHtmlElement :: forall m. MonadEffect m => HaloT m (Maybe WH.HTMLElement)
getTablatureContainerHtmlElement = do
  mode <- viewState _mode
  case mode of
    EditMode -> H.getHTMLElementRef refTablatureEditor
    ViewMode -> H.getHTMLElementRef refTablatureViewer
    SearchMode -> pure Nothing

getTablatureContainerElement :: forall m. MonadEffect m => HaloT m (Maybe Element)
getTablatureContainerElement = getTablatureContainerHtmlElement <#> \maybeHtmlElement -> maybeHtmlElement <#> toElement

saveScrollTop :: forall m. MonadEffect m => HaloT m Unit
saveScrollTop = do
  maybeTablatureContainerElem <- getTablatureContainerElement
  case maybeTablatureContainerElem of
    Nothing -> liftEffect $ Console.error "Could not find tablatureContainer to save scroll top"
    Just tablatureContainerElem -> do
      newScrollTop <- liftEffect $ scrollTop tablatureContainerElem
      setState _scrollTop newScrollTop

loadScrollTop :: forall m. MonadEffect m => HaloT m Unit
loadScrollTop = do
  maybeTablatureContainerElem <- getTablatureContainerElement
  case maybeTablatureContainerElem of
    Nothing -> liftEffect $ Console.error "Could not find tablatureContainer to load scroll top"
    Just tablatureContainerElem -> do
      scrollTop <- viewState _scrollTop
      liftEffect $ setScrollTop scrollTop tablatureContainerElem

focusSearchInput :: forall m. MonadEffect m => HaloT m Unit
focusSearchInput = do
  maybeSearchInputElement <- getSearchInputHtmlElement
  case maybeSearchInputElement of
    Nothing -> liftEffect $ Console.error "Could not find search element to focus"
    Just searchInputElement -> liftEffect $ focus searchInputElement

focusTablatureContainer :: forall m. MonadEffect m => HaloT m Unit
focusTablatureContainer = do
  maybeTablatureContainerElem <- getTablatureContainerHtmlElement
  case maybeTablatureContainerElem of
    Nothing -> liftEffect $ Console.error "Could not find tablatureContainer to focus"
    Just tablatureContainerElem -> liftEffect $ focus tablatureContainerElem

setTablatureTextInEditor :: forall m. MonadEffect m => String -> HaloT m Unit
setTablatureTextInEditor text = do
  maybeTextArea <- getTablatureEditorElement
  case maybeTextArea of
    Nothing -> liftEffect $ Console.error "Could not find textareaTablature to set text" *> pure unit
    Just textArea -> liftEffect $ WH.HTMLTextAreaElement.setValue text textArea

stopAutoscroll :: forall m. MonadEffect m => MonadState State m => m Unit
stopAutoscroll = do
  autoscrollTimer <- viewState _autoscrollTimer
  case autoscrollTimer of
    Nothing -> pure unit
    Just intervalId -> do
      liftEffect $ clearInterval intervalId
      setState _autoscrollTimer Nothing

startAutoscrollOnElement :: forall m. MonadEffect m => MonadState State m => Element -> m Unit
startAutoscrollOnElement elem = do
  autoscrollSpeed <- viewState _autoscrollSpeed
  intervalId <- liftEffect $ setInterval (speedToIntervalMs autoscrollSpeed) $ scrollBy 0 (speedToIntervalPixelDelta autoscrollSpeed) elem
  setState _autoscrollTimer (Just intervalId)
