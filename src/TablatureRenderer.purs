module TablatureRenderer where

import Prelude

import AppState (ChordLegendElem(..), ChordLineElem(..), HeaderLineElem(..), RenderingOptions, TablatureDocument, TablatureDocumentLine(..), TablatureLineElem(..), TextLineElem(..), TitleLineElem(..), Chord)
import Data.Array (fromFoldable)
import Data.Filterable (filterMap)
import Data.Foldable (foldr)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (length)
import Data.String.Utils (repeat)
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
  renderTextLineElem :: TextLineElem -> HH.HTML w i
  renderTextLineElem (Text string) = renderWithClass string "tabText"
  renderTextLineElem (Spaces string) = renderWithClass string "tabText"
  renderTextLineElem (TextLineChord chord) = renderChord chord
  renderTextLineElem (ChordLegend legend) = HH.span_ $ fromFoldable $ map renderChordLegendElem legend
  renderTablatureLineElem (Prefix string) = renderWithClass string "tabPrefix"
  renderTablatureLineElem (Suffix string) = renderWithClass string "tabSuffix"
  renderTablatureLineElem (Special string) = renderWithClass string "tabSpecial"
  renderTablatureLineElem (Timeline string) = renderWithClass string "tabTimeline"
  renderTablatureLineElem (Fret string) = renderWithClass string "tabFret"
  renderHeaderLineElem (Header string) = renderWithClass string "tabHeader"
  renderHeaderLineElem (HeaderSuffix string) = renderWithClass string "tabText"
  renderChordLineElem (ChordComment string) = renderWithClass string "tabSuffix"
  renderChordLineElem (ChordLineChord chord) = renderChord chord
  renderChordLegendElem :: ChordLegendElem -> HH.HTML w i
  renderChordLegendElem (ChordFret string) = renderWithClass string "tabFret"
  renderChordLegendElem (ChordSpecial string) = renderWithClass string "tabSpecial"
  renderChord :: Chord -> HH.HTML w i
  renderChord chord =
    HH.span [ classString "tabChord" ] $
      filterMap identity
        [ Just $ HH.text $ show chord.root.letter
        , Just $ HH.sub_ [ HH.text chord.root.mod ]
        , Just $ HH.text chord.type
        , Just $ HH.sup_ [ HH.text chordMods ]
        , chord.bass <#> \{ letter } -> HH.text $ "/" <> show letter
        , chord.bass <#> \{ mod } -> HH.sub_ [ HH.text mod ]
        , Just $ HH.text $ fromMaybe "" $ repeat chord.spaceSuffix " "
        , Just $ HH.sub_ [ createFontSizeCompensation chord.root.mod ]
        , Just $ HH.sup_ [ createFontSizeCompensation chordMods ]
        , chord.bass <#> \{ mod } -> HH.sub_ [ createFontSizeCompensation mod ]
        ]
    where
    createFontSizeCompensation string = HH.span [ classString "fontsize-compensation" ] [ HH.text $ fromMaybe "" $ repeat (length string) " " ]
    chordMods = foldr (<>) "" $ map show chord.mods

  renderWithClass string klass = HH.span [ classString klass ] [ HH.text string ]
