module TablatureRewriter where

import Prelude

import AppState (Chord(..), ChordMod(..), Note(..), RenderingOptions, Spaced(..), TablatureDocument, TablatureDocumentLine(..), TablatureLineElem(..), Transposition(..), _ChordLine, _ChordLineChord, _TextLine, _TextLineChord, _mod, _root, _TablatureLine, _Tuning)
import Data.Int (decimal, fromString, radix, toStringAs)
import Data.Lens (over, traversed)
import Data.List (List, reverse)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Data.String (Pattern(..), Replacement(..), replace, replaceAll)
import Data.String.CodePoints (stripPrefix)
import Data.String.CodeUnits (charAt, length)
import Data.String.Utils (repeat)
import Data.Tuple (Tuple(..))
import Utils (applyUntilIdempotent, foreach, pred', succ', print, class Print)

type TablatureDocumentRewriter = RenderingOptions -> TablatureDocument -> TablatureDocument

-- TODO: transpose bass notes
-- TODO: recognize false positives for chords in text and revert them to regular text.
-- TODO: dozenalize chord legends
-- TODO: rewrite every operation with lenses

rewriteTablatureDocument :: TablatureDocumentRewriter
rewriteTablatureDocument renderingOptions =
  fixEmDashes renderingOptions >>>
  addMissingClosingPipe renderingOptions >>>
  dozenalizeChords renderingOptions >>>
  dozenalizeFrets renderingOptions >>>
  transposeChords renderingOptions >>>
  transposeTuning renderingOptions

-- Map the Spaced version of some element and compensate the space suffix for the change in printed length
liftMappingSpaced :: forall a. (Print a) => (a -> a) -> ((Spaced a) -> (Spaced a))
liftMappingSpaced mapping (Spaced spaced) = Spaced { elem: newElem, spaceSuffix: newSuffix }
  where
  newElem = mapping spaced.elem
  newSuffix = spaced.spaceSuffix + length (print spaced.elem) - length (print newElem)

-- compensates for negative and positive space
applyChordMapping :: (Chord -> Chord) -> TablatureDocument -> TablatureDocument
applyChordMapping chordMapping = map (rewriteChordsInTextLine >>> rewriteChordsInChordLine)
  where
  mapping = liftMappingSpaced chordMapping

  rewriteChordsInTextLine :: TablatureDocumentLine -> TablatureDocumentLine
  rewriteChordsInTextLine = over (_ChordLine <<< traversed <<< _ChordLineChord) mapping

  rewriteChordsInChordLine :: TablatureDocumentLine -> TablatureDocumentLine
  rewriteChordsInChordLine = over (_TextLine <<< traversed <<< _TextLineChord) mapping

transposeChords :: TablatureDocumentRewriter
transposeChords renderingOptions = applyChordMapping $ chordMapping
  where
  chordMapping = over _root noteMapping
  noteMapping = transposeNote renderingOptions.transposition >>> canonicalizeNote

transposeTuning :: TablatureDocumentRewriter
transposeTuning renderingOptions = map rewriteLine
  where
  rewriteLine = over (_TablatureLine <<< traversed <<< _Tuning) (liftMappingSpaced noteMapping)
  noteMapping = transposeNote renderingOptions.transposition >>> canonicalizeNote

transposeNote :: Transposition -> Note -> Note
transposeNote transposition =
  case transposition of
    Transposition 0 -> identity
    Transposition t | t > 0 -> (appendSuffix $ fromMaybe "" $ repeat t "#") >>> canonicalizeNote
    Transposition t -> (appendSuffix $ fromMaybe "" $ repeat (abs t) "b") >>> canonicalizeNote
  where
  appendSuffix suffix = over _mod (_ <> suffix)

-- Rewrite the notes such that there is at most one # or b
canonicalizeNote :: Note -> Note
canonicalizeNote = applyUntilIdempotent rewrite1 >>> applyUntilIdempotent rewrite2 >>>  applyUntilIdempotent rewrite3
  where
  rewrite1 (Note note) = Note note { mod = rewriteMod note.mod }
    where rewriteMod = replace (Pattern "#b") (Replacement "") >>> replace (Pattern "b#") (Replacement "")
  rewrite2 (Note note@{ letter, mod }) = 
    case stripPrefix (Pattern "##") mod of
      Nothing -> Note note
      Just newMod -> Note note { letter = succ' letter, mod = newMod }
  rewrite3 (Note note@{ letter, mod }) = 
    case stripPrefix (Pattern "bb") mod of
      Nothing -> Note note
      Just newMod -> Note note { letter = pred' letter, mod = newMod }

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
  rewriteChord (Chord chord) = Chord chord { type = newType, mods = newMods }
    where
    newType = dozenalize chord.type
    newMods = map (\(ChordMod mod) -> ChordMod mod { interval = dozenalize mod.interval }) chord.mods

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
