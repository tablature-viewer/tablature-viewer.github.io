module Main where

import Prelude

import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console as Console
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import LZString (compressToEncodedURIComponent, decompressFromEncodedURIComponent)
import LocationString (getFragmentString, setFragmentString)
import Web.HTML as WH
import Web.HTML.HTMLTextAreaElement as WH.HTMLTextAreaElement

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Mode = ViewMode | EditMode
type State = { mode :: Mode, tablature :: String}
data Action = Initialize | ToggleMode

instance showMode :: Show Mode where
  show ViewMode = "View Mode"
  show EditMode = "Edit Mode"

otherMode :: Mode -> Mode
otherMode EditMode = ViewMode
otherMode ViewMode = EditMode

refTablatureEditor :: H.RefLabel
refTablatureEditor = H.RefLabel "tablatureEditor"

component :: forall query input output m. MonadEffect m => H.Component query input output m
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
initialState _ = { mode: EditMode, tablature: "" }

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div 
  [ HP.classes [ HH.ClassName "main" ] ]
  [ renderHeader
  , renderControls
  , renderTablature
  ]
  where
  renderHeader = HH.div
    [ HP.classes [ HH.ClassName "header" ] ]
    [ HH.h1_ [ HH.text "Dozenal Tablature Viewer" ] ]
  renderControls = HH.div 
    [ HP.classes [ HH.ClassName "controls" ] ]
    [ HH.button [ HE.onClick \_ -> ToggleMode ] [ HH.text $ show $ otherMode state.mode ] ]
  renderTablature = case state.mode of
    ViewMode -> HH.div 
      [ HP.classes [ HH.ClassName "tablatureViewer" ] ]
      [ HH.pre_ [ HH.text state.tablature ] ]
    EditMode -> HH.div 
      [ HP.classes [ HH.ClassName "tablatureEditor" ] ]
      [ HH.textarea
        [ HP.ref refTablatureEditor
        , HP.placeholder "Paste your plaintext tablature here"
        ]
      ]

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction action =
  case action of
    Initialize -> do
      string <- H.liftEffect getTablatureTextFromFragment
      state <- H.get
      H.put { mode: state.mode, tablature: string }
      setTablatureEditorText string
    ToggleMode -> do
      state <- H.get
      case state.mode of
        EditMode -> do
          saveTablature
          H.modify_ _ { mode = ViewMode }
        ViewMode -> do
          H.modify_ _ { mode = EditMode }
          setTablatureEditorText state.tablature

getTablatureEditorElement :: forall output m. H.HalogenM State Action () output m (Maybe WH.HTMLTextAreaElement)
getTablatureEditorElement = H.getHTMLElementRef refTablatureEditor <#>
  \maybeElement -> maybeElement >>= WH.HTMLTextAreaElement.fromHTMLElement

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
  saveTablatureToFragment tablatureText
  where
  saveTablatureToState tablatureText = do
    state <- H.get
    H.put { mode: state.mode, tablature: tablatureText }
  saveTablatureToFragment tablatureText = do
    case compressToEncodedURIComponent tablatureText of
      Just compressed -> H.liftEffect $ setFragmentString compressed
      Nothing -> H.liftEffect $ Console.error("Could not save tablature to URL")

getTablatureTextFromFragment :: Effect String
getTablatureTextFromFragment = do
  fragment <- H.liftEffect getFragmentString
  case decompressFromEncodedURIComponent fragment of
    Just decompressed -> pure $ decompressed
    Nothing ->  Console.error("Could not load tablature from URL") *> pure ""

setTablatureEditorText :: forall output m . MonadEffect m => String -> H.HalogenM State Action () output m Unit
setTablatureEditorText text = do
  maybeTextArea <- getTablatureEditorElement
  case maybeTextArea of
    Nothing -> H.liftEffect $ Console.error "Could not find textareaTablature" *> pure unit
    Just textArea -> H.liftEffect $ WH.HTMLTextAreaElement.setValue text textArea 
