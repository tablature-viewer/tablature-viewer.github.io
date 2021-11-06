module Main where

import Prelude
import AppState

import AppUrl (redirectToUrlInFragment)
import AutoscrollSpeed (speedToIntervalMs, speedToIntervalPixelDelta)

import Cache as Cache
import Clipboard (copyToClipboard)
import Data.Array (fromFoldable)
import Data.Enum (pred, succ)
import Data.Lens.Barlow (key)
import Data.Lens.Barlow.Helpers (view)
import Data.Maybe (Maybe(..))
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
import LocationString (getLocationString, getQueryParam)
import TablatureDocument (predTransposition, succTransposition)
import TablatureRenderer (renderTablatureDocument)
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

refTablatureEditor :: H.RefLabel
refTablatureEditor = H.RefLabel "tablatureEditor"

refTablatureViewer :: H.RefLabel
refTablatureViewer = H.RefLabel "tablatureViewer"

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


renderTablature :: forall w i. State -> Array (HH.HTML w i)
renderTablature state = fromFoldable $ renderTablatureDocument (Cache.peek _rewriteResult state)


modifyState :: forall output m . MonadEffect m => (StateRecord -> StateRecord) -> H.HalogenM State Action () output m Unit
modifyState f = H.modify_ \(State s) -> State (f s)

getState :: forall output m . MonadEffect m => H.HalogenM State Action () output m StateRecord
getState = do
  State state <- H.get
  pure state

handleAction :: forall output m . MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction action = do
  originalState <- getState
  modifyState _ { loading = true, autoscroll = false }
  updateAutoscrollTimer
  H.liftAff $ delay $ Milliseconds 0.0 -- TODO: this shouldn't be necessary to force rerender
  -- TODO: force halogen to render at this point, then pause and then resume at the end of the function again
  case action of
    Initialize -> do
      tablatureText <- Cache.getM _tablatureText
      if tablatureText == ""
      then modifyState _ { mode = EditMode }
      else modifyState _ { mode = ViewMode }
      focusTablatureContainer
    ToggleEditMode -> do
      saveScrollTop
      case originalState.mode of
        EditMode -> do
          tablatureText <- getTablatureTextFromEditor
          Cache.setM _tablatureText tablatureText
          modifyState _ { mode = ViewMode }
          focusTablatureContainer
        ViewMode -> do
          modifyState _ { mode = EditMode }
          tablatureText <- Cache.getM _tablatureText
          setTablatureTextInEditor tablatureText
          -- Don't focus the textarea, as the cursor position will be put at the end (which also sometimes makes the window jump)
      loadScrollTop
    ToggleTabNormalization -> do
      tabNormalizationEnabled <- Cache.getM _tabNormalizationEnabled
      Cache.setM _tabNormalizationEnabled (not tabNormalizationEnabled)
    ToggleTabDozenalization -> do
      ignoreDozenalization <- Cache.getM _ignoreDozenalization
      if ignoreDozenalization
      then pure unit
      else do
        tabDozenalizationEnabled <- Cache.getM _tabDozenalizationEnabled
        Cache.setM _tabDozenalizationEnabled (not tabDozenalizationEnabled)
    ToggleChordDozenalization -> do
      ignoreDozenalization <- Cache.getM _ignoreDozenalization
      if ignoreDozenalization
      then pure unit
      else do
        chordDozenalizationEnabled <- Cache.getM _chordDozenalizationEnabled
        Cache.setM _chordDozenalizationEnabled (not chordDozenalizationEnabled)
    CopyShortUrl -> do
      longUrl <- H.liftEffect getLocationString
      maybeShortUrl <- H.liftAff $ createShortUrl longUrl
      H.liftEffect $ case maybeShortUrl of
        Just shortUrl -> copyToClipboard shortUrl
        Nothing -> pure unit
      focusTablatureContainer
    ToggleAutoscroll -> modifyState _ { autoscroll = not originalState.autoscroll }
    IncreaseAutoscrollSpeed -> do
      increaseAutoscrollSpeed
      modifyState _ { autoscroll = originalState.autoscroll } -- We don't want to stop
    DecreaseAutoscrollSpeed -> do
      decreaseAutoscrollSpeed
      modifyState _ { autoscroll = originalState.autoscroll } -- We don't want to stop
    IncreaseTransposition -> do
      transposition <- Cache.getM _transposition
      Cache.setM _transposition $ succTransposition transposition
    DecreaseTransposition -> do
      transposition <- Cache.getM _transposition
      Cache.setM _transposition $ predTransposition transposition

  updateAutoscrollTimer

  tablatureTitle <- Cache.getM _tablatureTitle
  H.liftEffect $ setDocumentTitle tablatureTitle

  -- TODO: find a generic solution to preload cache
  _ <- Cache.getM _rewriteResult
  modifyState _ { loading = false }

getTablatureEditorElement :: forall output m. H.HalogenM State Action () output m (Maybe WH.HTMLTextAreaElement)
getTablatureEditorElement = H.getHTMLElementRef refTablatureEditor <#>
  \maybeHtmlElement -> maybeHtmlElement >>= WH.HTMLTextAreaElement.fromHTMLElement

getTablatureTextFromEditor :: forall output m . MonadEffect m => H.HalogenM State Action () output m String
getTablatureTextFromEditor = do
  maybeTextArea <- getTablatureEditorElement
  case maybeTextArea of
    Nothing -> H.liftEffect $ Console.error "Could not find textareaTablature" *> pure ""
    Just textArea -> H.liftEffect $ WH.HTMLTextAreaElement.value textArea 

getTablatureContainerElement :: forall output m. MonadEffect m => H.HalogenM State Action () output m (Maybe WH.HTMLElement)
getTablatureContainerElement = do
  state <- getState
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
      modifyState _ { scrollTop = newScrollTop }

loadScrollTop :: forall output m . MonadEffect m => H.HalogenM State Action () output m Unit
loadScrollTop = do
  state <- getState
  maybeTablatureContainerElem <- getTablatureContainerElement <#> \maybeHtmlElement -> maybeHtmlElement <#> toElement
  case maybeTablatureContainerElem of
    Nothing -> H.liftEffect $ Console.error "Could not find tablatureContainer"
    Just tablatureContainerElem -> do
      H.liftEffect $ setScrollTop state.scrollTop tablatureContainerElem

updateAutoscrollTimer :: forall output m . MonadEffect m => H.HalogenM State Action () output m Unit
updateAutoscrollTimer = do
  state <- getState
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
    modifyState _ { autoscrollTimer = Nothing }
  startAutoscroll = do
    state <- getState
    maybeTablatureContainerElem <- getTablatureContainerElement <#> \maybeHtmlElement -> maybeHtmlElement <#> toElement
    case maybeTablatureContainerElem of
      Nothing -> pure unit
      Just elem -> do
        -- TODO: compensate scrollspeed for fontsize differences between media
        timerId <- H.liftEffect $ setInterval (speedToIntervalMs state.autoscrollSpeed) $ scrollBy 0 (speedToIntervalPixelDelta state.autoscrollSpeed) elem
        modifyState _ { autoscrollTimer = Just timerId }

-- TODO: store scrollspeed somewhere
increaseAutoscrollSpeed :: forall output m . MonadEffect m => H.HalogenM State Action () output m Unit
increaseAutoscrollSpeed = do
  state <- getState
  case succ state.autoscrollSpeed of
    Nothing -> pure unit
    Just speed -> modifyState _ { autoscrollSpeed = speed }

decreaseAutoscrollSpeed :: forall output m . MonadEffect m => H.HalogenM State Action () output m Unit
decreaseAutoscrollSpeed = do
  state <- getState
  case pred state.autoscrollSpeed of
    Nothing -> pure unit
    Just speed -> modifyState _ { autoscrollSpeed = speed }

focusTablatureContainer :: forall output m . MonadEffect m => H.HalogenM State Action () output m Unit
focusTablatureContainer = do
  maybeTablatureContainerElem <- getTablatureContainerElement
  case maybeTablatureContainerElem of
    Nothing -> H.liftEffect $ Console.error "Could not find tablatureContainer"
    Just tablatureContainerElem -> H.liftEffect $ focus tablatureContainerElem


setTablatureTextInEditor :: forall output m . MonadEffect m => String -> H.HalogenM State Action () output m Unit
setTablatureTextInEditor text = do
  maybeTextArea <- getTablatureEditorElement
  case maybeTextArea of
    Nothing -> H.liftEffect $ Console.error "Could not find textareaTablature" *> pure unit
    Just textArea -> H.liftEffect $ WH.HTMLTextAreaElement.setValue text textArea 


setDocumentTitle :: String -> Effect Unit
setDocumentTitle title = do
    window <- window
    document <- document window
    setTitle title document
