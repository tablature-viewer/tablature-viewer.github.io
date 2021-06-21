module TablatureRenderer where

import Prelude

import AppState (TablatureDocument, TablatureDocumentLine(..), TablatureElem(..))
import HalogenUtils (classString)
import Data.Array (fromFoldable)
import Data.Foldable (foldl)
import Data.Int (decimal, radix, toStringAs)
import Data.List (List(..), reverse, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String.Utils (repeat)
import Halogen.HTML as HH

renderTablature :: forall w i. Maybe TablatureDocument -> Boolean -> String -> List (HH.HTML w i)
renderTablature maybeTablatureDoc dozenalize tablatureText = case maybeTablatureDoc of
  Nothing -> HH.text tablatureText : Nil
  Just tablatureDoc -> renderTablatureDocument tablatureDoc dozenalize

renderTablatureDocument :: forall w i. TablatureDocument -> Boolean -> List (HH.HTML w i)
renderTablatureDocument doc dozenalize = map renderLine doc
  where
  renderLine :: TablatureDocumentLine -> HH.HTML w i
  renderLine (CommentLine line) = HH.span [ classString "tabComment" ] [HH.text $ line <> "\n"]
  renderLine (TitleLine line) = HH.span_
    [ HH.span [ classString "tabComment" ] [HH.text $ line.prefix]
    , HH.span [ classString "tabTitle" ] [HH.text $ line.title]
    , HH.span [ classString "tabComment" ] [HH.text $ line.suffix <> "\n"]
    ]
  -- Rendering elements needs care because the numbers ↊ and ↋ take less space than 10 and 11.
  -- We need to make up for this with extra dashes at the first next Timeline element.
  renderLine (TablatureLine line) = HH.span_ $ fromFoldable $ (reverse $ result.acc) <> HH.text "\n" : Nil
    where result = foldl accTablatureElems {pendingDashes:0, acc: Nil} line
  accTablatureElems :: { acc:: List (HH.HTML w i), pendingDashes:: Int } -> TablatureElem -> { acc:: List (HH.HTML w i), pendingDashes:: Int } 
  accTablatureElems { pendingDashes, acc } elem = { pendingDashes: elemResult.pendingDashes, acc: (elemResult.result:acc) }
    where elemResult = renderTablatureElem dozenalize pendingDashes elem

type TablatureElemRenderResult w i = { result:: HH.HTML w i, pendingDashes:: Int }

renderTablatureElem :: forall w i. Boolean -> Int -> TablatureElem -> TablatureElemRenderResult w i
renderTablatureElem dozenalize pendingDashes (Prefix string) = { pendingDashes, result: HH.span [ classString "tabPrefix" ] [ HH.text string ] }
renderTablatureElem dozenalize pendingDashes (Suffix string) = { pendingDashes, result: HH.span [ classString "tabSuffix" ] [ HH.text string ] }
renderTablatureElem dozenalize pendingDashes (Special string) = { pendingDashes, result: HH.span [ classString "tabSpecial" ] [ HH.text string ] }
renderTablatureElem dozenalize pendingDashes (Timeline string) = { pendingDashes: 0, result: HH.span [ classString "tabTimeline" ] [ HH.text (fromMaybe "" (repeat pendingDashes "-") <> string) ] }
renderTablatureElem dozenalize pendingDashes (Fret n) =
  if dozenalize && (fretString == "↊" || fretString == "↋")
  then { pendingDashes: pendingDashes + 1, result: fretHtml }
  else { pendingDashes, result: fretHtml }
  where
  fretHtml = HH.span [ classString "tabFret" ] [ HH.text $ fretString ]
  fretString = if dozenalize then toDozenalString n else show n

toDozenalString :: Int -> String
toDozenalString n =
  -- If the fret number is large it's probably not a single fret number but rather incorrectly concatenated fret numbers.
  -- This already means that the tab is probably broken and ambiguous all over the place, but let's do a best effort
  -- of showing something understandable and not convert it.
  if n > 25
  then show n
  else toStringAs dozenal n # replaceAll (Pattern "a") (Replacement "↊") # replaceAll (Pattern "b") (Replacement "↋")
  where
  dozenal = fromMaybe decimal $ radix 12
