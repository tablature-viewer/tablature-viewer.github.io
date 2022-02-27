module TablatureRewriter where

import Prelude

import Data.Int (decimal, fromString, radix, toStringAs)
import Data.Lens (_Just, allOf, anyOf, over, set, traversed, view)
import Data.List (List, reverse)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Data.String (Pattern(..), Replacement(..), replace, replaceAll)
import Data.String.CodePoints (stripPrefix)
import Data.String.CodeUnits (charAt, length)
import Data.String.Utils (repeat)
import Data.Tuple (Tuple(..))
import TablatureDocument (Chord(..), ChordMod(..), Note, NoteLetterPrimitive(..), Spaced(..), TablatureDocument, TablatureDocumentLine(..), TablatureLineElem(..), TextLineElem(..), Transposition(..), _ChordLine, _ChordLineChord, _TablatureLine, _TextLine, _TextLineChord, _Tuning, _bass, _mod, _primitive, _root)
import Utils (class Print, applyUntilIdempotent, foreach, pred', print, succ', unsafeTestRegex)

type RewriteSettings =
  { dozenalizeTabs :: Boolean
  , dozenalizeChords :: Boolean
  , normalizeTabs :: Boolean
  , transposition :: Transposition
  }

type TablatureDocumentRewriter = RewriteSettings -> TablatureDocument -> TablatureDocument

-- TODO: dozenalize chord legends
-- TODO: rewrite every operation with lenses

rewriteTablatureDocument :: TablatureDocumentRewriter
rewriteTablatureDocument renderingOptions =
  revertFalsePositiveChords
    >>> fixEmDashes renderingOptions
    >>> addMissingClosingPipe renderingOptions
    >>> dozenalizeChords renderingOptions
    >>> dozenalizeFrets renderingOptions
    >>> transposeChords renderingOptions
    >>>
      transposeTuning renderingOptions

revertFalsePositiveChords :: TablatureDocument -> TablatureDocument
revertFalsePositiveChords = map rewriteLine
  where
  rewriteLine :: TablatureDocumentLine -> TablatureDocumentLine
  rewriteLine line =
    if allOf (_TextLine <<< traversed) (not elemIsChordLegend) line && anyOf (_TextLine <<< traversed) elemIsNaturalLanguage line then over (_TextLine <<< traversed) rewriteElement line
    else line

  elemIsNaturalLanguage :: TextLineElem -> Boolean
  elemIsNaturalLanguage = case _ of
    Text text -> unsafeTestRegex "^\\w*$" text
    _ -> false

  elemIsChordLegend :: TextLineElem -> Boolean
  elemIsChordLegend = case _ of
    ChordLegend _ -> true
    _ -> false

  rewriteElement :: TextLineElem -> TextLineElem
  rewriteElement = case _ of
    x@(TextLineChord spacedChord@(Spaced ({ elem: chord }))) ->
      if unsafeTestRegex "^([Aa][Mm]?|[DdGg]o|[a-z])$" $ print chord then Text $ print spacedChord
      else x
    x -> x

-- Map the Spaced version of some element and compensate the space suffix for the change in printed length
liftMappingSpaced :: forall a. Print a => (a -> a) -> ((Spaced a) -> (Spaced a))
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
  chordMapping = over _root noteMapping >>> over (_bass <<< _Just) noteMapping
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
canonicalizeNote =
  applyUntilIdempotent collapseRedundants
    >>> applyUntilIdempotent reduceSharps
    >>> applyUntilIdempotent reduceFlats
    >>>
      toPreferredMod
  where
  collapseRedundants note = note # over _mod (replace (Pattern "#b") (Replacement "") >>> replace (Pattern "b#") (Replacement ""))
  reduceSharps note =
    if view _primitive note == B || view _primitive note == E then substitute "#" else substitute "##"
    where
    substitute p = case stripPrefix (Pattern p) (view _mod note) of
      Nothing -> note
      Just newMod -> note # set _mod newMod # over _primitive succ'
  reduceFlats note =
    if view _primitive note == C || view _primitive note == F then substitute "b" else substitute "bb"
    where
    substitute p = case stripPrefix (Pattern p) (view _mod note) of
      Nothing -> note
      Just newMod -> note # set _mod newMod # over _primitive pred'
  toPreferredMod note =
    case view _mod note of
      "#" -> case view _primitive note of
        A -> note # set _mod "b" # set _primitive B
        D -> note # set _mod "b" # set _primitive E
        G -> note # set _mod "b" # set _primitive A
        _ -> note
      "b" -> case view _primitive note of
        D -> note # set _mod "#" # set _primitive C
        G -> note # set _mod "#" # set _primitive F
        _ -> note
      _ -> note

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
    foreach false (reverse elems)
      ( \done elem ->
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
  rewriteTablatureLineElems elems = foreach 0 elems
    ( \pendingDashes elem ->
        case elem of
          Timeline string -> Tuple 0 $ Timeline (fromMaybe "" (repeat pendingDashes "-") <> string)
          Fret string -> flip Tuple (Fret fretString) $
            if fretString == "↊" || fretString == "↋" then pendingDashes + 1
            else pendingDashes
            where
            fretString = toDozenalString string
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
        if n > 25 then show n
        else toStringAs dozenal n # replaceAll (Pattern "a") (Replacement "↊") # replaceAll (Pattern "b") (Replacement "↋")
        where
        dozenal = fromMaybe decimal $ radix 12
