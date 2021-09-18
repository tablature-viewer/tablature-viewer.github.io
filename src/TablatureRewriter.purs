module TablatureRewriter where

import Prelude

import AppState (RenderingOptions, TablatureDocument, TablatureDocumentLine(..), TablatureLineElem(..))
import Data.Foldable (foldl)
import Data.Int (decimal, fromString, radix, toStringAs)
import Data.List (List(..), reverse, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String.Utils (repeat)

type TablatureDocumentRewriter = RenderingOptions -> TablatureDocument -> TablatureDocument


rewriteTablatureDocument :: TablatureDocumentRewriter
rewriteTablatureDocument renderingOptions doc = doc # fixEmDashes renderingOptions # dozenalizeFrets renderingOptions

fixEmDashes :: TablatureDocumentRewriter
fixEmDashes renderingOptions doc = if not renderingOptions.normalize then doc else map rewriteLine doc
  where
  rewriteLine :: TablatureDocumentLine -> TablatureDocumentLine
  rewriteLine (TablatureLine line) = TablatureLine $ (map rewriteTablatureLineElem line)
  rewriteLine x = x

  rewriteTablatureLineElem :: TablatureLineElem -> TablatureLineElem
  rewriteTablatureLineElem (Timeline string) = Timeline $ rewriteTimeline string
  rewriteTablatureLineElem x = x

  rewriteTimeline string = replaceAll (Pattern "—") (Replacement "-") string

dozenalizeFrets :: TablatureDocumentRewriter
dozenalizeFrets renderingOptions doc = if not renderingOptions.dozenalize then doc else map rewriteLine doc
  where
  rewriteLine :: TablatureDocumentLine -> TablatureDocumentLine
  rewriteLine (TablatureLine line) = TablatureLine $ (rewriteTablatureLineElems line)
  rewriteLine x = x

  -- Rendering elements needs care because the numbers ↊ and ↋ take less space than 10 and 11.
  -- We need to make up for this with extra dashes at the first next Timeline element.
  rewriteTablatureLineElems :: List TablatureLineElem -> List TablatureLineElem
  rewriteTablatureLineElems elems = reverse result.acc
    where result = foldl (accTablatureLineElems ) {pendingDashes:0, acc: Nil} elems

  accTablatureLineElems :: { acc:: List TablatureLineElem, pendingDashes:: Int } -> TablatureLineElem -> { acc:: List TablatureLineElem, pendingDashes:: Int } 
  accTablatureLineElems { pendingDashes, acc } elem = { pendingDashes: elemResult.pendingDashes, acc: (elemResult.result:acc) }
    where elemResult = rewriteTablatureLineElem pendingDashes elem

  rewriteTablatureLineElem :: Int -> TablatureLineElem -> { result:: TablatureLineElem, pendingDashes:: Int }
  rewriteTablatureLineElem pendingDashes (Timeline string) =
    { pendingDashes: 0
    , result: Timeline (fromMaybe "" (repeat pendingDashes "-") <> string) }
  rewriteTablatureLineElem pendingDashes (Fret string) =
    if fretString == "↊" || fretString == "↋"
    then { pendingDashes: pendingDashes + 1, result: Fret fretString }
    else { pendingDashes, result: Fret fretString }
    where
    fretString = toDozenalString string
  rewriteTablatureLineElem pendingDashes elem = { pendingDashes, result: elem }

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