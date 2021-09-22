module TablatureRenderer where

import Prelude

import AppState (ChordLineElem(..), HeaderLineElem(..), TablatureDocument, TablatureDocumentLine(..), TablatureLineElem(..), TextLineElem(..), TitleLineElem(..), RenderingOptions)
import Data.Array (fromFoldable)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Utils (repeat)
import Data.String (length)
import Halogen.HTML as HH
import HalogenUtils (classString, renderLineEnding)

renderTablature :: forall w i. Maybe TablatureDocument -> String -> RenderingOptions -> List (HH.HTML w i)
renderTablature maybeTablatureDoc tablatureText renderingOptions = case maybeTablatureDoc of
  Nothing -> HH.text tablatureText : Nil
  Just tablatureDoc -> renderTablatureDocument tablatureDoc renderingOptions

renderTablatureDocument :: forall w i. TablatureDocument -> RenderingOptions -> List (HH.HTML w i)
renderTablatureDocument doc _ = map renderLine doc
  where
  renderLine :: TablatureDocumentLine -> HH.HTML w i
  renderLine (TitleLine line) = renderLine' line renderTitleLineElem
  renderLine (TextLine line) = renderLine' line renderTextLineElem
  renderLine (ChordLine line) = renderLine' line renderChordLineElem
  renderLine (HeaderLine line) = renderLine' line renderHeaderLineElem
  renderLine (TablatureLine line) = renderLine' line renderTablatureLineElem 

  renderLine' :: forall a. List a -> (a -> HH.HTML w i) -> HH.HTML w i
  renderLine' line lineRenderer = HH.span_ $ fromFoldable $ (lineRenderer <$> line) <> renderLineEnding : Nil

  renderTitleLineElem (Title string) = renderWithClass string "tabTitle"
  renderTitleLineElem (TitleOther string) = renderWithClass string "tabText"
  renderTextLineElem (Text string) = renderWithClass string "tabText"
  renderTablatureLineElem (Prefix string) = renderWithClass string "tabPrefix"
  renderTablatureLineElem (Suffix string) = renderWithClass string "tabSuffix"
  renderTablatureLineElem (Special string) = renderWithClass string "tabSpecial"
  renderTablatureLineElem (Timeline string) = renderWithClass string "tabTimeline"
  renderTablatureLineElem (Fret string) = renderWithClass string "tabFret"
  renderHeaderLineElem (Header string) = renderWithClass string "tabHeader"
  renderHeaderLineElem (HeaderSuffix string) = renderWithClass string "tabText"
  renderChordLineElem (ChordComment string) = renderWithClass string "tabSuffix"
  renderChordLineElem (ChordLegend string) = renderWithClass string "tabFret"
  renderChordLineElem (Chord chord) =
    HH.span [ classString "tabChord" ] [HH.text chord.root
    , HH.sub_ [ HH.text chord.rootMod, createFontSizeCompensation chord.rootMod ]
    , HH.text chord.type
    , HH.sup_ [ HH.text chord.mods, createFontSizeCompensation chord.mods ]
    , HH.text chord.bass
    , HH.sub_ [ HH.text chord.bassMod, createFontSizeCompensation chord.bassMod ]
    ]
    where createFontSizeCompensation string = HH.span [ classString "fontsize-compensation" ] [ HH.text $ fromMaybe "" $ repeat (length string) " " ]

  renderWithClass string klass = HH.span [ classString klass ] [ HH.text string ]
