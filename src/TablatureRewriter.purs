module TablatureRewriter where

import Prelude

import AppState (Chord, ChordLineElem(..), ChordMod(..), Note, RenderingOptions, TablatureDocument, TablatureDocumentLine(..), TablatureLineElem(..), TextLineElem(..), Transposition(..))
import Data.Foldable (foldr)
import Data.Int (decimal, fromString, radix, toStringAs)
import Data.List (List, reverse)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Data.String (Pattern(..), Replacement(..), replace, replaceAll)
import Data.String.CodePoints (stripPrefix)
import Data.String.CodeUnits (charAt, length)
import Data.String.Utils (repeat, filter)
import Data.Tuple (Tuple(..))
import Utils (applyUntilIdempotent, foreach, pred', succ')

type TablatureDocumentRewriter = RenderingOptions -> TablatureDocument -> TablatureDocument

-- TODO: recognize false positives for chords in text and revert them to regular text.

rewriteTablatureDocument :: TablatureDocumentRewriter
rewriteTablatureDocument renderingOptions =
  fixEmDashes renderingOptions >>>
  addMissingClosingPipe renderingOptions >>>
  dozenalizeChords renderingOptions >>>
  dozenalizeFrets renderingOptions >>>
  transposeChords renderingOptions

type ChordMapping = Chord -> Chord

-- TODO: compensate for negative and positive space
applyChordMapping :: ChordMapping -> TablatureDocument -> TablatureDocument
applyChordMapping chordMapping doc = map rewriteLine doc
  where
  rewriteLine :: TablatureDocumentLine -> TablatureDocumentLine
  rewriteLine (ChordLine line) = ChordLine $ (map rewriteChordLineElem line)
  rewriteLine (TextLine line) = TextLine $ (map rewriteTextLineElem line)
  rewriteLine x = x

  rewriteChordLineElem :: ChordLineElem -> ChordLineElem
  rewriteChordLineElem (ChordLineChord chord) = ChordLineChord $ chordMapping chord
  rewriteChordLineElem x = x

  rewriteTextLineElem :: TextLineElem -> TextLineElem
  rewriteTextLineElem (TextLineChord chord) = TextLineChord $ chordMapping chord
  rewriteTextLineElem x = x

transposeChords :: TablatureDocumentRewriter
transposeChords renderingOptions = applyChordMapping $ getChordMapping renderingOptions.transposition
  where
  getChordMapping transposition = 
    case transposition of
      Transposition 0 -> identity
      Transposition t | t > 0 -> (replaceSuffix $ fromMaybe "" $ repeat t "#") >>> canonicalizeRoot
      Transposition t ->  (replaceSuffix $ fromMaybe "" $ repeat (abs t) "b") >>> canonicalizeRoot
    where
    replaceSuffix suffix = \chord -> chord { root { mod = chord.root.mod <> suffix } }
    canonicalizeRoot = \chord -> chord { root = canonicalizeNote chord.root }

-- Rewrite the notes such that there is at most one # or b
canonicalizeNote :: Note -> Note
canonicalizeNote = applyUntilIdempotent rewrite1 >>> applyUntilIdempotent rewrite2 >>>  applyUntilIdempotent rewrite3
  where
  -- TODO: repeat until no changes anymore
  rewrite1 note = note { mod = rewriteMod note.mod }
    where rewriteMod = replace (Pattern "#b") (Replacement "") >>> replace (Pattern "b#") (Replacement "")
  rewrite2 note@{ letter, mod } = 
    case stripPrefix (Pattern "##") mod of
      Nothing -> note
      Just newMod -> note { letter = succ' letter, mod = newMod }
  rewrite3 note@{ letter, mod } = 
    case stripPrefix (Pattern "bb") mod of
      Nothing -> note
      Just newMod -> note { letter = pred' letter, mod = newMod }

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
dozenalizeChords renderingOptions doc = if not renderingOptions.dozenalizeChords then doc else applyChordMapping rewriteChord doc
  where
  rewriteChord :: Chord -> Chord
  rewriteChord chord = chord { type = newType, mods = newMods, spaceSuffix = chord.spaceSuffix + shrunkChars }
    where
    -- compensate for each 11 converted to ↋ by adding spaces after the bass mod
    -- TODO: clean up
    newType = dozenalize chord.type
    newMods = map (\(ChordMod mod) -> ChordMod mod { interval = dozenalize mod.interval }) chord.mods
    shrunkChars = (newStringWithAllTheNumbers # countShrunkChars) - (oldStringWithAllTheNumbers # countShrunkChars)
    oldStringWithAllTheNumbers = (chord.type <> foldr (<>) "" (map (\(ChordMod mod) -> mod.interval) chord.mods))
    newStringWithAllTheNumbers = (newType <> foldr (<>) "" (map (\(ChordMod mod) -> mod.interval) newMods))
    countShrunkChars s = s # filter (\c -> c == "↋" || c == "↊") # length

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
