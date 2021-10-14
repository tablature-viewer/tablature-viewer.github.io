module Main where

import Prelude

import AppState (Action(..), AutoscrollSpeed(..), Mode(..), RenderingOptions, State, TablatureDocument, TablatureDocumentLine(..), TitleLineElem(..), speedToIntervalMs, speedToIntervalPixelDelta)
import AppUrl (getTablatureTextFromUrl, redirectToUrlInFragment, saveTablatureToUrl)
import Clipboard (copyToClipboard)
import Data.Array (fromFoldable)
import Data.Enum (pred, succ)
import Data.List (List, findIndex, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Regex (test)
import Data.String.Regex.Flags (ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console as Console
import Effect.Timer (clearInterval, setInterval)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import HalogenUtils (classString, fontAwesome, optionalText, scrollBy)
import LocalStorage (getLocalStorageBoolean, setLocalStorage)
import LocationString (getLocationString, getQueryParam)
import TablatureParser (tryParseTablature)
import TablatureRenderer (renderTablature)
import TablatureRewriter (rewriteTablatureDocument)
import UrlShortener (createShortUrl)
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

defaultTitle :: String
defaultTitle = "Tab Viewer"

findElement :: forall a. (a -> Boolean) -> List a -> Maybe a
findElement p l =
  case findIndex p l of
    Nothing -> Nothing
    Just index -> case l !! index of
      Nothing -> Nothing
      Just elem -> Just elem

getTitle :: TablatureDocument -> String
getTitle tablatureDocument = 
  case findElement isTitleLine tablatureDocument of
    Nothing -> defaultTitle
    Just (TitleLine line) ->
      case findElement isTitle line of
        Nothing -> defaultTitle
        Just (Title title) -> title
        Just _ -> defaultTitle
    Just _ -> defaultTitle
  where
  isTitleLine (TitleLine _) = true
  isTitleLine _ = false
  isTitle (Title _) = true
  isTitle _ = false

getIgnoreDozenalization :: TablatureDocument -> Boolean
getIgnoreDozenalization tablatureDocument = test (unsafeRegex "dozenal" ignoreCase) (getTitle tablatureDocument)

getRenderingOptions :: State -> RenderingOptions
getRenderingOptions state =
  { dozenalizeTabs: state.tabDozenalizationEnabled && not state.ignoreDozenalization
  , dozenalizeChords: state.chordDozenalizationEnabled && not state.ignoreDozenalization
  , normalizeTabs: state.tabNormalizationEnabled }

refTablatureEditor :: H.RefLabel
refTablatureEditor = H.RefLabel "tablatureEditor"

refTablatureViewer :: H.RefLabel
refTablatureViewer = H.RefLabel "tablatureViewer"

localStorageKeyTabNormalizationEnabled :: String
localStorageKeyTabNormalizationEnabled = "tabNormalizationEnabled"

localStorageKeyTabDozenalizationEnabled :: String
localStorageKeyTabDozenalizationEnabled = "tabDozenalizationEnabled"

localStorageKeyChordDozenalizationEnabled :: String
localStorageKeyChordDozenalizationEnabled = "ChordDozenalizationEnabled"

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval 
      { handleAction = handleAction 
      , initialize = Just Initialize
      }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div_
  [ HH.div 
    [ classString "app" ]
    [ renderHeader
    , renderTablature
    ]
  , HH.div
    [ classString "tablaturePrinter tablature" ]
    [ HH.pre_ $ renderTablatureText state ]
  ]
  where
  renderTablature = case state.mode of
    ViewMode -> HH.div 
      [ classString "tablatureViewer tablature"
      , HP.ref refTablatureViewer
      , HP.tabIndex 0
      ]
      [ HH.pre_ $ renderTablatureText state ]
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
    if state.loading then [ HH.div_ [], HH.div_ [], HH.div_ [], HH.div_ [] ] else []
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
          [ if state.tabNormalizationEnabled
              then fontAwesome "fa-toggle-on"
              else fontAwesome "fa-toggle-off"
          , HH.text " Normalize tabs"
          ]
        , HH.button
          [ HP.title "Toggle decimal to dozenal conversion for tabs on or off"
          , classString "dropdown-item"
          , HE.onClick \_ -> ToggleTabDozenalization
          , classString $ if state.ignoreDozenalization then "disabled" else ""
          ]
          [ if state.tabDozenalizationEnabled && not state.ignoreDozenalization
              then fontAwesome "fa-toggle-on"
              else fontAwesome "fa-toggle-off"
          , HH.text " Dozenalize tabs"
          ]
        , HH.button
          [ HP.title "Toggle decimal to dozenal conversion for chords on or off"
          , HE.onClick \_ -> ToggleChordDozenalization
          , classString "dropdown-item"
          , classString $ if state.ignoreDozenalization then "disabled" else ""
          ]
          [ if state.chordDozenalizationEnabled && not state.ignoreDozenalization
              then fontAwesome "fa-toggle-on"
              else fontAwesome "fa-toggle-off"
          , HH.text " Dozenalize chords"
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
          , HH.span_ [ HH.text $ " Autoscroll speed " <> show state.autoscrollSpeed ]
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
    toggleButtonContent = case state.mode of
      EditMode -> [ fontAwesome "fa-save", optionalText " Save" ]
      ViewMode  -> [ fontAwesome "fa-edit", optionalText " Edit" ]
    toggleButtonTitle = case state.mode of
      EditMode -> "Save tablature"
      ViewMode  -> "Edit tablature"
    toggleAutoscrollContent =
      if state.autoscroll
      then [ fontAwesome "fa-stop", optionalText " Autoscroll" ]
      else [ fontAwesome "fa-play", optionalText " Autoscroll" ]


renderTablatureText :: forall w i. State -> Array (HH.HTML w i)
renderTablatureText state = fromFoldable $ renderTablature state.tablatureDocument state.tablatureText $ getRenderingOptions state

initialState :: forall input. input -> State
initialState _ = _initialState

_initialState :: State
_initialState =
  { mode: EditMode
  , loading: false
  , tablatureText: ""
  , tablatureTitle: defaultTitle
  , tablatureDocument: Nothing
  , scrollTop: 0.0
  , autoscroll: false
  , autoscrollTimer: Nothing
  , autoscrollSpeed: Normal
  , tabNormalizationEnabled: true
  , tabDozenalizationEnabled: false
  , chordDozenalizationEnabled: false
  , ignoreDozenalization: false }

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction action = do
  originalState <- H.get
  H.modify_ _ { loading = true, autoscroll = false }
  intermediateState <- H.get
  updateAutoscroll intermediateState
  H.liftAff $ delay $ Milliseconds 0.0 -- TODO: this shouldn't be necessary to force rerender
  case action of
    Initialize -> do
      maybeTabNormalizationEnabled <- H.liftEffect $ getLocalStorageBoolean localStorageKeyTabNormalizationEnabled
      tabNormalizationEnabled <- pure $ fromMaybe originalState.tabNormalizationEnabled maybeTabNormalizationEnabled
      maybeTabDozenalizationEnabled <- H.liftEffect $ getLocalStorageBoolean localStorageKeyTabDozenalizationEnabled
      tabDozenalizationEnabled <- pure $ fromMaybe originalState.tabDozenalizationEnabled maybeTabDozenalizationEnabled
      maybeChordDozenalizationEnabled <- H.liftEffect $ getLocalStorageBoolean localStorageKeyChordDozenalizationEnabled
      chordDozenalizationEnabled <- pure $ fromMaybe originalState.chordDozenalizationEnabled maybeChordDozenalizationEnabled
      H.put $ _initialState { scrollTop = originalState.scrollTop, tabNormalizationEnabled = tabNormalizationEnabled, tabDozenalizationEnabled = tabDozenalizationEnabled, chordDozenalizationEnabled = chordDozenalizationEnabled }
      maybeTablatureText <- H.liftEffect getTablatureTextFromUrl
      case maybeTablatureText of
        Just tablatureText -> do
          case readTablature tablatureText { dozenalizeTabs: tabDozenalizationEnabled, dozenalizeChords: chordDozenalizationEnabled, normalizeTabs: tabNormalizationEnabled } of
            Just tablatureDocument -> do
              H.modify_ _ { mode = ViewMode, tablatureText = tablatureText, tablatureTitle = tablatureTitle, tablatureDocument = Just tablatureDocument, ignoreDozenalization = ignoreDozenalization }
              H.liftEffect $ setDocumentTitle tablatureTitle
              where
                tablatureTitle = getTitle tablatureDocument
                ignoreDozenalization = getIgnoreDozenalization tablatureDocument
            Nothing ->
              H.modify_ _ { tablatureText = tablatureText }
        Nothing -> pure unit
      focusTablatureContainer
    ToggleEditMode -> do
      saveScrollTop
      case originalState.mode of
        EditMode -> do
          saveTablature
          H.modify_ _ { mode = ViewMode }
          focusTablatureContainer
        ViewMode -> do
          H.modify_ _ { mode = EditMode, tablatureDocument = Nothing }
          setTablatureEditorText originalState.tablatureText
          -- Don't focus the textarea, as the cursor position will be put at the end (which also sometimes makes the window jump)
      loadScrollTop
    ToggleTabNormalization -> do
      H.liftEffect $ setLocalStorage localStorageKeyTabNormalizationEnabled (show $ not originalState.tabNormalizationEnabled)
      H.modify_ _ { tabNormalizationEnabled = not originalState.tabNormalizationEnabled }
      -- Normalization affects the TablatureDocument, so we need to re-read it now
      case originalState.mode of
        ViewMode -> readTablatureAndSaveToState originalState.tablatureText
        _ -> pure unit
    ToggleTabDozenalization -> do
      if originalState.ignoreDozenalization
      then pure unit
      else do
        H.liftEffect $ setLocalStorage localStorageKeyTabDozenalizationEnabled (show $ not originalState.tabDozenalizationEnabled)
        H.modify_ _ { tabDozenalizationEnabled = not originalState.tabDozenalizationEnabled }
        -- Dozenalization affects the TablatureDocument, so we need to re-read it now
        case originalState.mode of
          ViewMode -> readTablatureAndSaveToState originalState.tablatureText
          _ -> pure unit
    ToggleChordDozenalization -> do
      if originalState.ignoreDozenalization
      then pure unit
      else do
        H.liftEffect $ setLocalStorage localStorageKeyChordDozenalizationEnabled (show $ not originalState.chordDozenalizationEnabled)
        H.modify_ _ { chordDozenalizationEnabled = not originalState.chordDozenalizationEnabled }
        -- Dozenalization affects the TablatureDocument, so we need to re-read it now
        case originalState.mode of
          ViewMode -> readTablatureAndSaveToState originalState.tablatureText
          _ -> pure unit
    CopyShortUrl -> do
      longUrl <- H.liftEffect getLocationString
      maybeShortUrl <- H.liftAff $ createShortUrl longUrl
      H.liftEffect $ case maybeShortUrl of
        Just shortUrl -> copyToClipboard shortUrl
        Nothing -> pure unit
      focusTablatureContainer
    ToggleAutoscroll -> H.modify_ _ { autoscroll = not originalState.autoscroll }
    IncreaseAutoscrollSpeed -> do
      increaseAutoscrollSpeed originalState
      H.modify_ _ { autoscroll = originalState.autoscroll }
    DecreaseAutoscrollSpeed -> do
      decreaseAutoscrollSpeed originalState
      H.modify_ _ { autoscroll = originalState.autoscroll }

  newState <- H.get
  updateAutoscroll newState
  H.modify_ _ { loading = false }

getTablatureContainerElement :: forall output m. H.HalogenM State Action () output m (Maybe WH.HTMLElement)
getTablatureContainerElement = do
  state <- H.get
  case state.mode of
    EditMode -> H.getHTMLElementRef refTablatureEditor
    ViewMode -> H.getHTMLElementRef refTablatureViewer

saveScrollTop :: forall output m . MonadEffect m => H.HalogenM State Action () output m Unit
saveScrollTop = do
  maybeTablatureContainerElem <- getTablatureContainerElement <#> \maybeHtmlElement -> maybeHtmlElement <#> toElement
  case maybeTablatureContainerElem of
    Nothing -> H.liftEffect $ Console.error "Could not find tablatureContainer"
    Just tablatureContainerElem -> do
      newScrollTop <- H.liftEffect $ scrollTop tablatureContainerElem
      H.modify_ _ { scrollTop = newScrollTop }

loadScrollTop :: forall output m . MonadEffect m => H.HalogenM State Action () output m Unit
loadScrollTop = do
  state <- H.get
  maybeTablatureContainerElem <- getTablatureContainerElement <#> \maybeHtmlElement -> maybeHtmlElement <#> toElement
  case maybeTablatureContainerElem of
    Nothing -> H.liftEffect $ Console.error "Could not find tablatureContainer"
    Just tablatureContainerElem -> do
      H.liftEffect $ setScrollTop state.scrollTop tablatureContainerElem

updateAutoscroll :: forall output m . MonadEffect m => State -> H.HalogenM State Action () output m Unit
updateAutoscroll state =
  case state.autoscrollTimer of
    Nothing ->
      if state.autoscroll
      then startAutoscroll
      else pure unit
    Just timerId ->
      if state.autoscroll
      then pure unit
      else stopAutoscroll timerId
  where
  stopAutoscroll timerId = do
    H.liftEffect $ clearInterval timerId
    H.modify_ _ { autoscrollTimer = Nothing }
  startAutoscroll = do
    maybeTablatureContainerElem <- getTablatureContainerElement <#> \maybeHtmlElement -> maybeHtmlElement <#> toElement
    case maybeTablatureContainerElem of
      Nothing -> pure unit
      Just elem -> do
        -- TODO: compensate scrollspeed for fontsize differences between media
        timerId <- H.liftEffect $ setInterval (speedToIntervalMs state.autoscrollSpeed) $ scrollBy 0 (speedToIntervalPixelDelta state.autoscrollSpeed) elem
        H.modify_ _ { autoscrollTimer = Just timerId }

-- TODO: store scrollspeed in url parameter
increaseAutoscrollSpeed :: forall output m . MonadEffect m => State -> H.HalogenM State Action () output m Unit
increaseAutoscrollSpeed state = do
  case succ state.autoscrollSpeed of
    Nothing -> pure unit
    Just speed -> H.modify_ _ { autoscrollSpeed = speed }

decreaseAutoscrollSpeed :: forall output m . MonadEffect m => State -> H.HalogenM State Action () output m Unit
decreaseAutoscrollSpeed state = do
  case pred state.autoscrollSpeed of
    Nothing -> pure unit
    Just speed -> H.modify_ _ { autoscrollSpeed = speed }

focusTablatureContainer :: forall output m . MonadEffect m => H.HalogenM State Action () output m Unit
focusTablatureContainer = do
  maybeTablatureContainerElem <- getTablatureContainerElement
  case maybeTablatureContainerElem of
    Nothing -> H.liftEffect $ Console.error "Could not find tablatureContainer"
    Just tablatureContainerElem -> H.liftEffect $ focus tablatureContainerElem


getTablatureEditorElement :: forall output m. H.HalogenM State Action () output m (Maybe WH.HTMLTextAreaElement)
getTablatureEditorElement = H.getHTMLElementRef refTablatureEditor <#>
  \maybeHtmlElement -> maybeHtmlElement >>= WH.HTMLTextAreaElement.fromHTMLElement

getTablatureEditorText :: forall output m . MonadEffect m => H.HalogenM State Action () output m String
getTablatureEditorText = do
  maybeTextArea <- getTablatureEditorElement
  case maybeTextArea of
    Nothing -> H.liftEffect $ Console.error "Could not find textareaTablature" *> pure ""
    Just textArea -> H.liftEffect $ WH.HTMLTextAreaElement.value textArea 

saveTablature :: forall output m . MonadEffect m => H.HalogenM State Action () output m Unit
saveTablature = do
  tablatureText <- getTablatureEditorText
  readTablatureAndSaveToState tablatureText
  state <- H.get
  H.liftEffect $ setDocumentTitle state.tablatureTitle
  saveTablatureToUrl

readTablature :: String -> RenderingOptions -> Maybe TablatureDocument
readTablature tablatureText renderingOptions = do
  case tryParseTablature tablatureText of
    Just parseResult -> Just $ rewriteTablatureDocument renderingOptions parseResult
    Nothing -> Nothing

readTablatureAndSaveToState :: forall output m . MonadEffect m => String -> H.HalogenM State Action () output m Unit
readTablatureAndSaveToState tablatureText = do
  state <- H.get
  case readTablature tablatureText (getRenderingOptions state) of
    Just tablatureDocument ->
      H.modify_ _ { tablatureText = tablatureText, tablatureTitle = getTitle tablatureDocument, tablatureDocument = Just tablatureDocument, ignoreDozenalization = getIgnoreDozenalization tablatureDocument }
    Nothing ->
      H.modify_ _ { tablatureText = tablatureText, tablatureTitle = defaultTitle, tablatureDocument = Nothing }

setTablatureEditorText :: forall output m . MonadEffect m => String -> H.HalogenM State Action () output m Unit
setTablatureEditorText text = do
  maybeTextArea <- getTablatureEditorElement
  case maybeTextArea of
    Nothing -> H.liftEffect $ Console.error "Could not find textareaTablature" *> pure unit
    Just textArea -> H.liftEffect $ WH.HTMLTextAreaElement.setValue text textArea 

setDocumentTitle :: String -> Effect Unit
setDocumentTitle title = do
    window <- window
    document <- document window
    setTitle title document
