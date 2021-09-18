module TablatureRewriter where

import Prelude

import AppState (RenderingOptions, TablatureDocument, TablatureDocumentLine(..), TablatureLineElem(..))
import Data.Foldable (foldl)
import Data.Int (decimal, fromString, radix, toStringAs)
import Data.List (List(..), reverse, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String.Utils (repeat)

rewriteTablatureDocument :: TablatureDocument -> RenderingOptions -> TablatureDocument
rewriteTablatureDocument doc renderingOptions = map rewriteLine doc
  where
  rewriteLine :: TablatureDocumentLine -> TablatureDocumentLine
  rewriteLine (TablatureLine line) = TablatureLine $ (rewriteTablatureLineElems renderingOptions line)
  rewriteLine x = x

-- Rendering elements needs care because the numbers ↊ and ↋ take less space than 10 and 11.
-- We need to make up for this with extra dashes at the first next Timeline element.
rewriteTablatureLineElems :: RenderingOptions -> List TablatureLineElem -> List TablatureLineElem
rewriteTablatureLineElems renderingOptions elems = reverse result.acc
  where result = foldl (accTablatureLineElems renderingOptions) {pendingDashes:0, acc: Nil} elems

accTablatureLineElems :: RenderingOptions -> { acc:: List TablatureLineElem, pendingDashes:: Int } -> TablatureLineElem -> { acc:: List TablatureLineElem, pendingDashes:: Int } 
accTablatureLineElems renderingOptions { pendingDashes, acc } elem = { pendingDashes: elemResult.pendingDashes, acc: (elemResult.result:acc) }
  where elemResult = rewriteTablatureLineElem renderingOptions pendingDashes elem

type TablatureLineElemRenderResult = { result:: TablatureLineElem, pendingDashes:: Int }

rewriteTablatureLineElem :: RenderingOptions -> Int -> TablatureLineElem -> TablatureLineElemRenderResult
rewriteTablatureLineElem renderingOptions pendingDashes (Timeline string) =
  { pendingDashes: 0
  , result: Timeline (fromMaybe "" (repeat pendingDashes "-") <> rewriteTimeline renderingOptions.normalize string) }
rewriteTablatureLineElem renderingOptions pendingDashes (Fret string) =
  if renderingOptions.dozenalize && (fretString == "↊" || fretString == "↋")
  then { pendingDashes: pendingDashes + 1, result: Fret fretString }
  else { pendingDashes, result: Fret fretString }
  where
  fretString = if renderingOptions.dozenalize then toDozenalString string else string
rewriteTablatureLineElem _ pendingDashes elem = { pendingDashes, result: elem }

-- Replace em dashes by normal dashes
rewriteTimeline :: Boolean -> String -> String
rewriteTimeline normalize string = if normalize then replaceAll (Pattern "—") (Replacement "-") string else string

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
