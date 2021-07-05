module TablatureRenderer where

import Prelude

import AppState (ChordLineElem(..), HeaderLineElem(..), TablatureDocument, TablatureDocumentLine(..), TablatureLineElem(..), TextLineElem(..), TitleLineElem(..))
import Data.Array (fromFoldable)
import Data.Foldable (foldl)
import Data.Int (decimal, fromString, radix, toStringAs)
import Data.List (List(..), reverse, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String.Utils (repeat)
import Halogen.HTML as HH
import HalogenUtils (classString, renderLineEnding)

renderTablature :: forall w i. Maybe TablatureDocument -> Boolean -> String -> List (HH.HTML w i)
renderTablature maybeTablatureDoc dozenalize tablatureText = case maybeTablatureDoc of
  Nothing -> HH.text tablatureText : Nil
  Just tablatureDoc -> renderTablatureDocument tablatureDoc dozenalize

renderTablatureDocument :: forall w i. TablatureDocument -> Boolean -> List (HH.HTML w i)
renderTablatureDocument doc dozenalize = map renderLine doc
  where
  renderLine :: TablatureDocumentLine -> HH.HTML w i
  renderLine (TitleLine line) = HH.span_ $ fromFoldable $ (renderTitleLineElem <$> line) <> renderLineEnding : Nil
  renderLine (TextLine line) = HH.span_ $ fromFoldable $ (renderTextLineElem <$> line) <> renderLineEnding : Nil
  renderLine (ChordLine line) = HH.span_ $ fromFoldable $ (renderChordLineElem <$> line) <> renderLineEnding : Nil
  renderLine (HeaderLine line) = HH.span_ $ fromFoldable $ (renderHeaderLineElem <$> line) <> renderLineEnding : Nil
  renderLine (TablatureLine line) = HH.span_ $ fromFoldable $ (renderTablatureLineElems dozenalize line) <> renderLineEnding : Nil

  renderTitleLineElem (Title text) = HH.span [ classString "tabTitle" ] [ HH.text text ]
  renderTitleLineElem (TitleOther text) = HH.span [ classString "tabText" ] [ HH.text text ]
  renderTextLineElem (Text text) = HH.span [ classString "tabText" ] [ HH.text text ]
  renderChordLineElem (Chord text) = HH.span [ classString "tabChord" ] [ HH.text text ]
  renderHeaderLineElem (Header text) = HH.span [ classString "tabHeader" ] [ HH.text text ]
  renderHeaderLineElem (HeaderSuffix text) = HH.span [ classString "tabText" ] [ HH.text text ]

-- Rendering elements needs care because the numbers ↊ and ↋ take less space than 10 and 11.
-- We need to make up for this with extra dashes at the first next Timeline element.
renderTablatureLineElems :: forall w i. Boolean -> List TablatureLineElem -> List (HH.HTML w i)
renderTablatureLineElems dozenalize elems = reverse result.acc
  where result = foldl (accTablatureLineElems dozenalize) {pendingDashes:0, acc: Nil} elems

accTablatureLineElems :: forall w i. Boolean -> { acc:: List (HH.HTML w i), pendingDashes:: Int } -> TablatureLineElem -> { acc:: List (HH.HTML w i), pendingDashes:: Int } 
accTablatureLineElems dozenalize { pendingDashes, acc } elem = { pendingDashes: elemResult.pendingDashes, acc: (elemResult.result:acc) }
  where elemResult = renderTablatureLineElem dozenalize pendingDashes elem

type TablatureLineElemRenderResult w i = { result:: HH.HTML w i, pendingDashes:: Int }

renderTablatureLineElem :: forall w i. Boolean -> Int -> TablatureLineElem -> TablatureLineElemRenderResult w i
renderTablatureLineElem _ pendingDashes (Prefix string) = { pendingDashes, result: HH.span [ classString "tabPrefix" ] [ HH.text string ] }
renderTablatureLineElem _ pendingDashes (Suffix string) = { pendingDashes, result: HH.span [ classString "tabSuffix" ] [ HH.text string ] }
renderTablatureLineElem _ pendingDashes (Special string) = { pendingDashes, result: HH.span [ classString "tabSpecial" ] [ HH.text string ] }
renderTablatureLineElem _ pendingDashes (Timeline string) = { pendingDashes: 0, result: HH.span [ classString "tabTimeline" ] [ HH.text (fromMaybe "" (repeat pendingDashes "-") <> string) ] }
renderTablatureLineElem dozenalize pendingDashes (Fret string) =
  if dozenalize && (fretString == "↊" || fretString == "↋")
  then { pendingDashes: pendingDashes + 1, result: fretHtml }
  else { pendingDashes, result: fretHtml }
  where
  fretHtml = HH.span [ classString "tabFret" ] [ HH.text $ fretString ]
  fretString = if dozenalize then toDozenalString string else string

toDozenalString :: String -> String
toDozenalString s =
  case fromString s of
    Nothing -> s
    Just n ->
      -- If the fret number is large it's probably not a single fret number but rather incorrectly concatenated fret numbers.
      -- This already means that the tab is probably broken and ambiguous all over the place, but let's do a best effort
      -- of showing something understandable and not convert it.
      if n > 25
      then show n
      else toStringAs dozenal n # replaceAll (Pattern "a") (Replacement "↊") # replaceAll (Pattern "b") (Replacement "↋")
      where
      dozenal = fromMaybe decimal $ radix 12
