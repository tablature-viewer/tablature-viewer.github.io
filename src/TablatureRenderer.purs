module TablatureRenderer where

import Prelude
import AppState

import HalogenUtils (classString)
import Data.Array (fromFoldable)
import Data.Foldable (foldl)
import Data.Int (decimal, radix, toStringAs)
import Data.List (List(..), reverse, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String.Utils (repeat)
import Halogen.HTML as HH

renderTablature :: forall w i. Maybe TablatureDocument -> String -> List (HH.HTML w i)
renderTablature maybeTablatureDoc tablatureText = case maybeTablatureDoc of
  Nothing -> HH.text tablatureText : Nil
  Just tablatureDoc -> renderTablatureDocument tablatureDoc

renderTablatureDocument :: forall w i. TablatureDocument -> List (HH.HTML w i)
renderTablatureDocument doc = map renderLine doc
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
    where elemResult = renderTablatureElem pendingDashes elem

type TablatureElemRenderResult w i = { result:: HH.HTML w i, pendingDashes:: Int }

renderTablatureElem :: forall w i. Int -> TablatureElem -> TablatureElemRenderResult w i
renderTablatureElem pendingDashes (Prefix string) = { pendingDashes, result: HH.span [ classString "tabPrefix" ] [ HH.text string ] }
renderTablatureElem pendingDashes (Suffix string) = { pendingDashes, result: HH.span [ classString "tabSuffix" ] [ HH.text string ] }
renderTablatureElem pendingDashes (Special string) = { pendingDashes, result: HH.span [ classString "tabSpecial" ] [ HH.text string ] }
renderTablatureElem pendingDashes (Timeline string) = { pendingDashes: 0, result: HH.span [ classString "tabTimeline" ] [ HH.text (fromMaybe "" (repeat pendingDashes "-") <> string) ] }
renderTablatureElem pendingDashes (Fret n) =
  if dozenalString == "↊" || dozenalString == "↋"
  then { pendingDashes: pendingDashes + 1, result: fretHtml }
  else { pendingDashes, result: fretHtml }
  where
  fretHtml = HH.span [ classString "tabFret" ] [ HH.text $ dozenalString ]
  dozenalString = toDozenalString n

toDozenalString :: Int -> String
toDozenalString n = toStringAs dozenal n # replaceAll (Pattern "a") (Replacement "↊") # replaceAll (Pattern "b") (Replacement "↋")
  where
  dozenal = fromMaybe decimal $ radix 12
