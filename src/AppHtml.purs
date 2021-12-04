module AppHtml where

import AppActions
import AppState
import Prelude

import Cache as Cache
import Data.Array (fromFoldable)
import Data.Lens.Barlow (key)
import Data.Lens.Barlow.Helpers (view)
import DebugUtils (debug)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenUtils (classString, fontAwesome, optionalText)
import TablatureRenderer (renderTablatureDocument)


refTablatureEditor :: H.RefLabel
refTablatureEditor = H.RefLabel "tablatureEditor"

refTablatureViewer :: H.RefLabel
refTablatureViewer = H.RefLabel "tablatureViewer"

render :: forall m. State -> H.ComponentHTML Action () m
render state = debug "rendering" $ HH.div_
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
  renderBody = case view (key :: _ "!.mode") state of
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
      , HE.onClick \_ -> CreateShortUrl
      ] [ fontAwesome "fa-share", optionalText " Share" ]
    , HH.button
      [ HP.title "Toggle autoscrolling"
      , HE.onClick \_ -> ToggleAutoscroll
      ] toggleAutoscrollContent
    ]
    where
    toggleButtonContent = case view (key :: _ "!.mode") state of
      EditMode -> [ fontAwesome "fa-save", optionalText " Save" ]
      ViewMode -> [ fontAwesome "fa-edit", optionalText " Edit" ]
    toggleButtonTitle = case view (key :: _ "!.mode") state of
      EditMode -> "Save tablature"
      ViewMode -> "Edit tablature"
    toggleAutoscrollContent =
      if view (key :: _ "!.autoscroll") state
      then [ fontAwesome "fa-stop", optionalText " Autoscroll" ]
      else [ fontAwesome "fa-play", optionalText " Autoscroll" ]
