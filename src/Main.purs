module Main where

import Prelude

import AppState (Action(..), Mode(..), State, TablatureDocument, TablatureDocumentLine(..))
import AppUrl (getTablatureTextFromUrl, saveTablatureToUrl)

import Clipboard (copyToClipboard)
import Data.Array (fromFoldable)
import Data.List (findIndex, (!!))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console as Console
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import HalogenUtils (classString, fontAwesome, optionalText)
import LocationString (getLocationString)
import TablatureParser (tryParseTablature)
import TablatureRenderer (renderTablature)
import UrlShortener (createShortUrl)
import Web.DOM.Element (scrollTop, setScrollTop)
import Web.HTML (window)
import Web.HTML as WH
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.HTMLElement (focus, toElement)
import Web.HTML.HTMLTextAreaElement as WH.HTMLTextAreaElement
import Web.HTML.Window (document)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

defaultTitle :: String
defaultTitle = "Tab Viewer"

getTitle :: TablatureDocument -> String
getTitle tablatureDocument = 
  case findIndex isTitle tablatureDocument of
    Nothing -> defaultTitle
    Just index ->
      case tablatureDocument !! index of
        Just (TitleLine line) -> line.title
        Nothing -> defaultTitle
        Just _ -> defaultTitle
  where
  isTitle (TitleLine _) = true
  isTitle _ = false

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

initialState :: forall input. input -> State
initialState _ = { mode: EditMode, loading: false, tablatureText: "", tablatureTitle: defaultTitle, tablatureDocument: Nothing, scrollTop: 0.0 }

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div 
  [ classString "main" ]
  [ renderHeader
  , renderTablature
  ]
  where
  renderTablature = case state.mode of
        ViewMode -> HH.div 
          [ classString "tablatureViewer"
          , HP.ref refTablatureViewer
          , HP.tabIndex 0
          ]
          [ HH.pre_ $ renderTablatureText state ]
        EditMode -> HH.textarea
          [ HP.ref refTablatureEditor
          , classString "tablatureEditor" 
          , HP.placeholder "Paste your plaintext tablature here and click 'Save'"
          , HP.spellcheck false
          ]
  renderHeader = HH.div
    [ classString "header" ]
    [ renderTitle
    , renderControls
    , renderLoadingIcon
    ]
  renderTitle = HH.div
    [ classString "title optional"]
    [ HH.a
      [ HP.href "https://github.com/dznl/tabviewer"
      , HP.target "_blank"
      ]
      [ HH.h1_ [ HH.text "Dozenal Tablature Viewer" ] ]
    ]
  renderLoadingIcon = HH.div 
    [ classString "loadingIcon lds-ellipsis" ]
    if state.loading then [ HH.div_ [], HH.div_ [], HH.div_ [], HH.div_ [] ] else []
  renderControls = HH.div 
    [ classString "controls" ]
    [ HH.button [ HP.title toggleButtonTitle, HE.onClick \_ -> ToggleMode ] toggleButtonContent
    , HH.button
      [ HP.title "Create a short link to the tablature for sharing with other people"
      , HE.onClick \_ -> CopyShortUrl
      ] [ fontAwesome "fa-share", optionalText " Share" ]
    , HH.a
      [ HP.href "./"
      , HP.target "_blank"
      , HP.tabIndex (-1)
      ]
      [ HH.button
        [ HP.title "Open an empty tablature in a new browser tab" ] [ fontAwesome "fa-plus", optionalText " New" ] ]
    , HH.a
      [ HP.href "https://github.com/dznl/tabviewer"
      , HP.target "_blank"
      , HP.tabIndex (-1)
      ]
      [ HH.button [ HP.title "Open the README in a new browser tab" ] [ fontAwesome "fa-info", optionalText " About" ] ]
    ]
    where
    toggleButtonContent = case state.mode of
      EditMode -> [ fontAwesome "fa-save", optionalText " Save" ]
      ViewMode  -> [ fontAwesome "fa-edit", optionalText " Edit" ]
    toggleButtonTitle = case state.mode of
      EditMode -> "Save tablature"
      ViewMode  -> "Edit tablature"


renderTablatureText :: forall w i. State -> Array (HH.HTML w i)
renderTablatureText state = fromFoldable $ renderTablature state.tablatureDocument state.tablatureText

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction action =
  case action of
    Initialize -> do
      maybeTablatureText <- H.liftEffect getTablatureTextFromUrl
      state <- H.get
      case maybeTablatureText of
        Just tablatureText -> do
          case tryParseTablature tablatureText of
            Just tablatureDocument -> do
              H.put { mode: ViewMode, loading: false, tablatureText: tablatureText, tablatureTitle, tablatureDocument: Just tablatureDocument, scrollTop: state.scrollTop }
              H.liftEffect $ setDocumentTitle tablatureTitle
              where tablatureTitle = getTitle tablatureDocument
            Nothing ->
              H.put { mode: EditMode, loading: false, tablatureText: tablatureText, tablatureTitle: defaultTitle, tablatureDocument: Nothing, scrollTop: state.scrollTop }
        Nothing ->
          H.put { mode: EditMode, loading: false, tablatureText: "", tablatureTitle: defaultTitle, tablatureDocument: Nothing, scrollTop: state.scrollTop }
      focusTablatureContainer
    ToggleMode -> do
      H.modify_ _ { loading = true }
      H.liftAff $ delay $ Milliseconds 0.0 -- TODO: this shouldn't be necessary to force rerender
      saveScrollTop
      state <- H.get
      case state.mode of
        EditMode -> do
          saveTablature
          H.modify_ _ { mode = ViewMode }
          focusTablatureContainer
        ViewMode -> do
          H.modify_ _ { mode = EditMode, tablatureDocument = Nothing }
          setTablatureEditorText state.tablatureText
          -- Don't focus the textarea, as the cursor position will be put at the end (which also sometimes makes the window jump)
      loadScrollTop
      H.modify_ _ { loading = false }
    CopyShortUrl -> do
      H.modify_ _ { loading = true }
      longUrl <- H.liftEffect getLocationString
      maybeShortUrl <- H.liftAff $ createShortUrl longUrl
      H.modify_ _ { loading = false }
      H.liftAff $ delay $ Milliseconds 0.0 -- TODO: this shouldn't be necessary to force rerender
      H.liftEffect $ case maybeShortUrl of
        Just shortUrl -> copyToClipboard shortUrl
        Nothing -> pure unit
      focusTablatureContainer

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
  saveTablatureToState tablatureText
  state <- H.get
  H.liftEffect $ setDocumentTitle state.tablatureTitle
  saveTablatureToUrl
  where
  saveTablatureToState :: String -> H.HalogenM State Action () output m Unit
  saveTablatureToState tablatureText = do
    case tryParseTablature tablatureText of
      Just tablatureDocument ->
        H.modify_ _ { tablatureText = tablatureText, tablatureTitle = getTitle tablatureDocument, tablatureDocument = Just tablatureDocument }
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
