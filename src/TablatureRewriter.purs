module TablatureRewriter where

import Prelude

import AppState (ChordLineElem(..), RenderingOptions, TablatureDocument, TablatureDocumentLine(..), TablatureLineElem(..))
import Data.Int (decimal, fromString, radix, toStringAs)
import Data.List (List, reverse)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String.CodeUnits (charAt, length)
import Data.String.Utils (repeat, filter)
import Data.Tuple (Tuple(..))
import Utils (foreach)

type TablatureDocumentRewriter = RenderingOptions -> TablatureDocument -> TablatureDocument

-- TODO: recognize false positives for chords in text and revert them to regular text.

rewriteTablatureDocument :: TablatureDocumentRewriter
rewriteTablatureDocument renderingOptions =
  fixEmDashes renderingOptions >>>
  addMissingClosingPipe renderingOptions >>>
  dozenalizeChords renderingOptions >>>
  dozenalizeFrets renderingOptions

fixEmDashes :: TablatureDocumentRewriter
fixEmDashes renderingOptions doc = if not renderingOptions.normalizeTabs then doc else map rewriteLine doc
  where
  rewriteLine :: TablatureDocumentLine -> TablatureDocumentLine
  rewriteLine (TablatureLine line) = TablatureLine $ (map rewriteTablatureLineElem line)
  rewriteLine x = x

  rewriteTablatureLineElem :: TablatureLineElem -> TablatureLineElem
  rewriteTablatureLineElem (Timeline string) = Timeline $ replaceAll (Pattern "—") (Replacement "-") string
  rewriteTablatureLineElem x = x

addMissingClosingPipe :: TablatureDocumentRewriter
addMissingClosingPipe renderingOptions doc = if not renderingOptions.normalizeTabs then doc else map rewriteLine doc
  where
  rewriteLine :: TablatureDocumentLine -> TablatureDocumentLine
  rewriteLine (TablatureLine line) = TablatureLine $ rewriteTablatureLine line
  rewriteLine x = x

  rewriteTablatureLine :: List TablatureLineElem -> List TablatureLineElem
  rewriteTablatureLine elems = reverse $
    foreach false (reverse elems) (\done elem ->
      case elem of
        Timeline t -> Tuple true (Timeline (if done then t else rewriteLastTimelinePiece t))
        _ -> Tuple done elem
    )

  rewriteLastTimelinePiece :: String -> String
  rewriteLastTimelinePiece string = if charAt (length string - 1) string /= Just '|' then string <> "|" else string

dozenalizeChords :: TablatureDocumentRewriter
dozenalizeChords renderingOptions doc = if not renderingOptions.dozenalizeChords then doc else map rewriteLine doc
  where
  rewriteLine :: TablatureDocumentLine -> TablatureDocumentLine
  rewriteLine (ChordLine line) = ChordLine $ (map rewriteChordLineElem line)
  rewriteLine x = x

  rewriteChordLineElem :: ChordLineElem -> ChordLineElem
  rewriteChordLineElem (ChordLineChord chord) = ChordLineChord $ chord { type = newType, mods = newMods, bassMod = newBassMod }
    where
    -- compensate for each 11 converted to ↋ by adding spaces after the bass mod
    newType = dozenalize chord.type
    newMods = dozenalize chord.mods
    shrunkChars = (newType <> newMods) # filter (_ == "↋") # length
    newBassMod = chord.bassMod <> fromMaybe "" (repeat shrunkChars " ")
  rewriteChordLineElem x = x

  dozenalize = replaceAll (Pattern "11") (Replacement "↋") >>> replaceAll (Pattern "13") (Replacement "11") 

dozenalizeFrets :: TablatureDocumentRewriter
dozenalizeFrets renderingOptions doc = if not renderingOptions.dozenalizeTabs then doc else map rewriteLine doc
  where
  rewriteLine :: TablatureDocumentLine -> TablatureDocumentLine
  rewriteLine (TablatureLine line) = TablatureLine $ rewriteTablatureLineElems line
  rewriteLine x = x

  -- Rendering elements needs care because the numbers ↊ and ↋ take less space than 10 and 11.
  -- We need to make up for this with extra dashes at the first next Timeline element.
  rewriteTablatureLineElems :: List TablatureLineElem -> List TablatureLineElem
  rewriteTablatureLineElems elems = foreach 0 elems (\pendingDashes elem ->
    case elem of
      Timeline string -> Tuple 0 $ Timeline (fromMaybe "" (repeat pendingDashes "-") <> string)
      Fret string -> flip Tuple (Fret fretString) $
        if fretString == "↊" || fretString == "↋"
        then pendingDashes + 1
        else pendingDashes
        where fretString = toDozenalString string
      _ -> Tuple pendingDashes elem
    )

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
