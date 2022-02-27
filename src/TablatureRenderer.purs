module TablatureRenderer where

import Prelude
import TablatureDocument

import Data.Array (fromFoldable)
import Data.Filterable (filterMap)
import Data.Foldable (foldr)
import Data.Lens (view)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (length)
import Data.String.Utils (repeat)
import Halogen.HTML as HH
import HalogenUtils (classString, renderLineEnding)
import Utils (print)

renderTablatureDocument :: forall w i. TablatureDocument -> List (HH.HTML w i)
renderTablatureDocument doc = map renderLine doc
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
  renderTablatureLineElem (Tuning note) = renderTuning note
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

  renderTuning :: (Spaced Note) -> HH.HTML w i
  renderTuning spacedNote =
    HH.span [ classString "tabChord" ]
      [ HH.text $ print $ view (_elem <<< _letter) spacedNote
      , HH.sub_ [ HH.text $ view (_elem <<< _mod) spacedNote ]
      , HH.text $ fromMaybe "" $ repeat (view _spaceSuffix spacedNote) " "
      , HH.sub_ [ createFontSizeCompensation $ view (_elem <<< _mod) spacedNote ]
      ]

  renderChord :: (Spaced Chord) -> HH.HTML w i
  renderChord chord =
    HH.span [ classString "tabChord" ] $
      filterMap identity
        [ Just $ HH.text $ print $ view (_elem <<< _root <<< _letter) chord
        , Just $ HH.sub_ [ HH.text $ view (_elem <<< _root <<< _mod) chord ]
        , Just $ HH.text $ view (_elem <<< _type) chord
        , Just $ HH.sup_ [ HH.text chordMods ]
        , view (_elem <<< _bass) chord <#> \(Note { letter }) -> HH.text $ "/" <> print letter
        , view (_elem <<< _bass) chord <#> \(Note { mod }) -> HH.sub_ [ HH.text mod ]
        , Just $ HH.text $ fromMaybe "" $ repeat (max 1 (view _spaceSuffix chord)) " "
        , Just $ HH.sub_ [ createFontSizeCompensation $ view (_elem <<< _root <<< _mod) chord ]
        , Just $ HH.sup_ [ createFontSizeCompensation chordMods ]
        , view (_elem <<< _bass) chord <#> \(Note { mod }) -> HH.sub_ [ createFontSizeCompensation mod ]
        ]
    where
    chordMods = foldr (<>) "" $ map print (view (_elem <<< _mods) chord)
  createFontSizeCompensation string = HH.span [ classString "fontsize-compensation" ] [ HH.text $ fromMaybe "" $ repeat (length string) " " ]

  renderWithClass string klass = HH.span [ classString klass ] [ HH.text string ]
