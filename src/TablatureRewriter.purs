module TablatureRewriter where

import Prelude

import Data.Int (decimal, fromString, radix, toStringAs)
import Data.Lens (_Just, allOf, anyOf, filtered, lengthOf, over, set, traversed, view)
import Data.List (List, reverse)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Data.String (Pattern(..), Replacement(..), replace, replaceAll)
import Data.String.CodePoints (stripPrefix)
import Data.String.CodeUnits (charAt, length)
import Data.String.Utils (repeat)
import Data.Tuple (Tuple(..))
import TablatureDocument (Chord(..), ChordMod(..), Note, NoteLetterPrimitive(..), Spaced(..), TablatureDocument, TablatureDocumentLine(..), TablatureLineElem(..), TextLineElem(..), Transposition(..), _ChordLine, _ChordLineChord, _TablatureLine, _TextLine, _TextLineChord, _Tuning, _bass, _lowercase, _mod, _primitive, _root)
import Utils (class Print, applyUntilIdempotent, foreach, pred', print, succ', unsafeTestRegex)

type RewriteSettings =
  { dozenalizeTabs :: Boolean
  , dozenalizeChords :: Boolean
  , normalizeTabs :: Boolean
  , normalizeChords :: Boolean
  , transposition :: Transposition
  , noteOrientation :: NoteOrientation
  }

data NoteOrientation = Flat | Sharp | Default

derive instance Eq NoteOrientation

type TablatureDocumentRewriter = RewriteSettings -> TablatureDocument -> TablatureDocument

-- TODO: dozenalize chord legends
-- TODO: rewrite every operation with lenses

rewriteTablatureDocument :: TablatureDocumentRewriter
rewriteTablatureDocument settings =
  -- The order of the operations is important
  identity
    >>> revertFalsePositiveChords
    >>> fixTimeLine settings
    >>> addMissingClosingPipe settings
    >>> dozenalizeChords settings
    >>> dozenalizeFrets settings
    >>> transposeChords settings
    >>>
      transposeTuning settings

revertFalsePositiveChords :: TablatureDocument -> TablatureDocument
revertFalsePositiveChords = map rewriteLine
  where
  rewriteLine :: TablatureDocumentLine -> TablatureDocumentLine
  rewriteLine line =
    -- If the line contains at least one unambiguous chord or at least four ambiguous chords or a chord legend we don't revert anything
    if containsChordLegend line || containsAtLeastFourAmbiguousChords line || containsAtLeastOneUnambiguousChord line then line
    else over (_TextLine <<< traversed) rewriteElement line

  containsChordLegend line = anyOf (_TextLine <<< traversed) isChordLegend line
  containsAtLeastFourAmbiguousChords line = lengthOf (_TextLine <<< traversed <<< filtered isPotentiallyAmbiguousChord) line > 3
  containsAtLeastOneUnambiguousChord line = anyOf (_TextLine <<< traversed) isUnambiguousChord line

  isPotentiallyAmbiguousChord :: TextLineElem -> Boolean
  isPotentiallyAmbiguousChord = case _ of
    TextLineChord (Spaced ({ elem: chord })) -> unsafeTestRegex "^([Aa][Mm]?|[DdGg]o|[a-z]|Bob)$" $ print chord
    _ -> false

  isUnambiguousChord :: TextLineElem -> Boolean
  isUnambiguousChord elem = case elem of
    TextLineChord _ -> not (isPotentiallyAmbiguousChord elem)
    _ -> false

  isChordLegend :: TextLineElem -> Boolean
  isChordLegend = case _ of
    ChordLegend _ -> true
    _ -> false

  rewriteElement :: TextLineElem -> TextLineElem
  rewriteElement = case _ of
    x@(TextLineChord spacedChord) ->
      if isPotentiallyAmbiguousChord x then Text $ print spacedChord
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
transposeChords settings = applyChordMapping $ chordMapping
  where
  chordMapping = over _root noteMapping >>> over (_bass <<< _Just) noteMapping
  noteMapping = transposeNote settings >>> if settings.normalizeChords then canonicalizeNote settings else identity

transposeTuning :: TablatureDocumentRewriter
transposeTuning settings = map rewriteLine
  where
  rewriteLine = over (_TablatureLine <<< traversed <<< _Tuning) (liftMappingSpaced noteMapping)
  noteMapping = transposeNote settings >>> if settings.normalizeChords then canonicalizeNote settings else identity

transposeNote :: RewriteSettings -> Note -> Note
transposeNote settings =
  case settings.transposition of
    Transposition 0 -> identity
    Transposition t | t > 0 -> (appendSuffix $ fromMaybe "" $ repeat t "#") >>> canonicalizeNote settings
    Transposition t -> (appendSuffix $ fromMaybe "" $ repeat (abs t) "b") >>> canonicalizeNote settings
  where
  appendSuffix suffix = over _mod (_ <> suffix)

-- Rewrite the notes such that there is at most one # or b
canonicalizeNote :: RewriteSettings -> Note -> Note
canonicalizeNote settings =
  applyUntilIdempotent collapseRedundants
    >>> applyUntilIdempotent reduceSharps
    >>> applyUntilIdempotent reduceFlats
    >>> toPreferredOrientation
    >>> toUpperCase
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
  toPreferredOrientation note =
    case settings.noteOrientation of
      Default ->
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
      Flat ->
        case view _mod note of
          "#" -> case view _primitive note of
            A -> note # set _mod "b" # set _primitive B
            C -> note # set _mod "b" # set _primitive D
            D -> note # set _mod "b" # set _primitive E
            F -> note # set _mod "b" # set _primitive G
            G -> note # set _mod "b" # set _primitive A
            _ -> note
          _ -> note
      Sharp ->
        case view _mod note of
          "b" -> case view _primitive note of
            A -> note # set _mod "#" # set _primitive G
            B -> note # set _mod "#" # set _primitive A
            D -> note # set _mod "#" # set _primitive C
            E -> note # set _mod "#" # set _primitive D
            G -> note # set _mod "#" # set _primitive F
            _ -> note
          _ -> note
  toUpperCase note = note # set _lowercase false

fixTimeLine :: TablatureDocumentRewriter
fixTimeLine settings doc = if not settings.normalizeTabs then doc else map rewriteLine doc
  where
  rewriteLine :: TablatureDocumentLine -> TablatureDocumentLine
  rewriteLine (TablatureLine line) = TablatureLine $ (map rewriteTablatureLineElem line)
  rewriteLine x = x

  rewriteTablatureLineElem :: TablatureLineElem -> TablatureLineElem
  rewriteTablatureLineElem (TimelineConnection string) =
    TimelineConnection
      $ replaceAll (Pattern "—") (Replacement "-")
      $ replaceAll (Pattern " ") (Replacement "-")
      $ string
  rewriteTablatureLineElem (TimelineSep string) =
    TimelineSep
      $ replaceAll (Pattern "[") (Replacement "|")
      $ replaceAll (Pattern "]") (Replacement "|")
      $ string
  rewriteTablatureLineElem x = x

addMissingClosingPipe :: TablatureDocumentRewriter
addMissingClosingPipe settings doc = if not settings.normalizeTabs then doc else map rewriteLine doc
  where
  rewriteLine :: TablatureDocumentLine -> TablatureDocumentLine
  rewriteLine (TablatureLine line) = TablatureLine $ rewriteTablatureLine line
  rewriteLine x = x

  rewriteTablatureLine :: List TablatureLineElem -> List TablatureLineElem
  rewriteTablatureLine elems = reverse $
    foreach false (reverse elems)
      ( \done elem ->
          case elem of
            TimelineSep _ -> Tuple true elem
            -- TODO: this is not entirely correct. We should actually insert a TimelineSep element here
            TimelineConnection t -> Tuple true ((if done then elem else TimelineSep (rewriteLastTimelinePiece t)))
            _ -> Tuple done elem
      )

  rewriteLastTimelinePiece :: String -> String
  rewriteLastTimelinePiece string = if charAt (length string - 1) string /= Just '|' then string <> "|" else string

dozenalizeChords :: TablatureDocumentRewriter
dozenalizeChords settings doc = if not settings.dozenalizeChords then doc else applyChordMapping rewriteChord doc
  where
  rewriteChord :: Chord -> Chord
  rewriteChord (Chord chord) = Chord chord { type = newType, mods = newMods }
    where
    newType = dozenalize chord.type
    newMods = map (\(ChordMod mod) -> ChordMod mod { interval = dozenalize mod.interval }) chord.mods

  dozenalize = replaceAll (Pattern "11") (Replacement "↋") >>> replaceAll (Pattern "13") (Replacement "11")

dozenalizeFrets :: TablatureDocumentRewriter
dozenalizeFrets settings doc = if not settings.dozenalizeTabs then doc else map rewriteLine doc
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
          -- TODO: this is not entirely correct. We should actually insert a TimelineConnection element here
          TimelineConnection string -> Tuple 0 $ TimelineConnection (fromMaybe "" (repeat pendingDashes "-") <> string)
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
