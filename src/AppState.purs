module AppState where

import Prelude

import Data.Enum (class Enum, pred, succ)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', Prism', Traversal', over, prism', traversed)
import Data.Lens.Common (simple)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (toLower)
import Data.String.Utils (repeat)
import Data.Symbol (SProxy(..))
import Effect.Timer (IntervalId)
import Partial.Unsafe (unsafePartial)
import Utils (class Print, class CyclicEnum, pred', succ', print)

data Action
  = Initialize 
  | ToggleEditMode 
  | ToggleTabNormalization 
  | ToggleTabDozenalization 
  | ToggleChordDozenalization 
  | CopyShortUrl
  | ToggleAutoscroll
  | IncreaseAutoscrollSpeed
  | DecreaseAutoscrollSpeed
  | IncreaseTransposition
  | DecreaseTransposition

instance showMode :: Show Mode where
  show ViewMode = "View Mode"
  show EditMode = "Edit Mode"

data Mode = ViewMode | EditMode

data AutoscrollSpeed
  = Slowest
  | Slower
  | Slow
  | Normal
  | Fast
  | Fastest

instance showAutoscrollSpeed :: Show AutoscrollSpeed where
  show Slowest = "(0.2)"
  show Slower = "(0.4)"
  show Slow = "(0.6)"
  show Normal = "(1.0)"
  show Fast = "(2.0)"
  show Fastest = "(4.0)"

derive instance eqAutoscrollSpeed :: Eq AutoscrollSpeed
derive instance ordAutoscrollSpeed :: Ord AutoscrollSpeed
derive instance genericAutoscrollSpeed :: Generic AutoscrollSpeed _
instance enumAutoscrollSpeed :: Enum AutoscrollSpeed where
  succ = genericSucc
  pred = genericPred

speedToIntervalMs :: AutoscrollSpeed -> Int
speedToIntervalMs Slowest = 400
speedToIntervalMs Slower = 200
speedToIntervalMs Slow = 120
speedToIntervalMs Normal = 80
speedToIntervalMs Fast = 40
speedToIntervalMs Fastest = 40

speedToIntervalPixelDelta :: AutoscrollSpeed -> Int
speedToIntervalPixelDelta Slowest = 1
speedToIntervalPixelDelta Slower = 1
speedToIntervalPixelDelta Slow = 1
speedToIntervalPixelDelta Normal = 1
speedToIntervalPixelDelta Fast = 1
speedToIntervalPixelDelta Fastest = 2

type State =
  { mode :: Mode
  , loading :: Boolean
  , tablatureText :: String
  , tablatureTitle :: String
  , tablatureDocument :: Maybe TablatureDocument
  -- Store the scrollTop in the state before actions so we can restore the expected scrollTop when switching views
  , scrollTop :: Number
  , autoscrollTimer :: Maybe IntervalId
  , autoscrollSpeed :: AutoscrollSpeed
  , autoscroll :: Boolean
  , tabNormalizationEnabled :: Boolean
  , tabDozenalizationEnabled :: Boolean
  , chordDozenalizationEnabled :: Boolean
  -- For tabs that are already dozenal themselves we want to ignore any dozenalization settings
  , ignoreDozenalization :: Boolean
  , transposition :: Transposition
  }

type RenderingOptions =
  { dozenalizeTabs :: Boolean
  , dozenalizeChords :: Boolean
  , normalizeTabs :: Boolean
  , transposition :: Transposition
  }

type TablatureDocument = List TablatureDocumentLine

data TablatureDocumentLine
  = TitleLine (List TitleLineElem) -- Title of whole document
  | HeaderLine (List HeaderLineElem) -- Header per section of document
  | TablatureLine (List TablatureLineElem)
  | ChordLine (List ChordLineElem)
  | TextLine (List TextLineElem)

_TablatureLine :: Prism' TablatureDocumentLine (List TablatureLineElem)
_TablatureLine = prism' TablatureLine case _ of
  TablatureLine l -> Just l
  _ -> Nothing

_ChordLine :: Prism' TablatureDocumentLine (List ChordLineElem)
_ChordLine = prism' ChordLine case _ of
  ChordLine l -> Just l
  _ -> Nothing

_TextLine :: Prism' TablatureDocumentLine (List TextLineElem)
_TextLine = prism' TextLine case _ of
  TextLine l -> Just l
  _ -> Nothing

data TitleLineElem
  = Title String
  | TitleOther String

data HeaderLineElem
  = Header String
  | HeaderSuffix String

data TablatureLineElem
  = Prefix String
  | Tuning (Spaced Note)
  | Timeline String
  | Fret String
  | Special String
  | Suffix String

_Tuning :: Prism' TablatureLineElem (Spaced Note)
_Tuning = prism' Tuning case _ of
  Tuning l -> Just l
  _ -> Nothing

data TextLineElem
  = Text String
  | Spaces String
  | TextLineChord (Spaced Chord)
  | ChordLegend (List ChordLegendElem)

_TextLineChord :: Prism' TextLineElem (Spaced Chord)
_TextLineChord = prism' TextLineChord case _ of
  TextLineChord l -> Just l
  _ -> Nothing

data ChordLegendElem
  = ChordFret String
  | ChordSpecial String

data ChordLineElem
  = ChordLineChord (Spaced Chord)
  | ChordComment String

_ChordLineChord :: Prism' ChordLineElem (Spaced Chord)
_ChordLineChord = prism' ChordLineChord case _ of
  ChordLineChord l -> Just l
  _ -> Nothing

-- The number of spaces after an expression.
-- E.g. this is part of a chord so that it can be expanded and shrunk easily when rewriting chords without losing the original alignment
newtype Spaced a = Spaced { elem :: a, spaceSuffix :: Int }
derive instance newtypeSpaced :: Newtype (Spaced a) _

-- _elem :: forall a r. Lens' { elem :: a | r } a
_elem = simple _Newtype <<< prop (SProxy :: SProxy "elem")
_spaceSuffix = simple _Newtype <<< prop (SProxy :: SProxy "spaceSuffix")

instance printSpaced :: (Print a) => Print (Spaced a) where
  print (Spaced x) = print x.elem <> (fromMaybe "" $ repeat x.spaceSuffix " ")

newtype Chord = Chord
  { root :: Note
  , type :: String
  , mods :: List ChordMod
  , bass :: Maybe Note
  }
derive instance newtypeChord :: Newtype Chord _

-- _root :: forall n a r. Lens' (Newtype n { root :: a | r }) a
_root = _Newtype <<< prop (SProxy :: SProxy "root")
_type = _Newtype <<< prop (SProxy :: SProxy "type")
-- _bass :: forall a r. Lens' { bass :: a | r } a
_bass = _Newtype <<< prop (SProxy :: SProxy "bass")
-- _mods :: forall a r. Lens' { mods :: a | r } a
_mods = _Newtype <<< prop (SProxy :: SProxy "mods")

derive instance eqChord :: Eq Chord

instance printChord :: Print Chord where
  print (Chord chord) =
    (chord.root # print)
    <> chord.type
    <> (foldr (<>) "" (map (\(ChordMod mod) -> mod.pre <> mod.interval <> mod.post) chord.mods))
    <> (fromMaybe "" $ chord.bass <#> print)

instance printNote :: Print Note where
  print (Note note) = print note.letter <> note.mod

newtype ChordMod = ChordMod
  { pre :: String
  , interval :: String
  , post :: String
  }
instance printChordMod :: Print ChordMod where
  print (ChordMod mod) = mod.pre <> mod.interval <> mod.post

derive instance eqChordMod :: Eq ChordMod

newtype Note = Note
  { letter :: NoteLetter
  , mod :: String
  }
derive instance newtypeNote :: Newtype Note _

-- _letter :: forall a r. Lens' { letter :: a | r } a
_letter = _Newtype <<< prop (SProxy :: SProxy "letter")
-- _mod :: forall a r. Lens' { mod :: a | r } a
_mod = _Newtype <<< prop (SProxy :: SProxy "mod")

derive instance eqNote :: Eq Note

newtype NoteLetter = NoteLetter
  { primitive :: NoteLetterPrimitive
  , lowercase :: Boolean
  }

data NoteLetterPrimitive = A | B | C | D | E | F | G

derive instance eqNoteLetterPrimitive :: Eq NoteLetterPrimitive
derive instance ordNoteLetterPrimitive :: Ord NoteLetterPrimitive
derive instance genericNoteLetterPrimitive :: Generic NoteLetterPrimitive _
instance enumNoteLetterPrimitive :: Enum NoteLetterPrimitive where
  succ G = Just A
  succ x = genericSucc x
  pred A = Just G
  pred x = genericPred x

instance cyclicEnumNoteLetterPrimitive :: CyclicEnum NoteLetterPrimitive where
  succ' x = unsafePartial $ fromJust $ succ x
  pred' x = unsafePartial $ fromJust $ pred x

instance noteLetterPrimitivePrint :: Print NoteLetterPrimitive where
  print = genericShow

fromString :: String -> Maybe NoteLetterPrimitive
fromString "A" = Just A
fromString "B" = Just B
fromString "C" = Just C
fromString "D" = Just D
fromString "E" = Just E
fromString "F" = Just F
fromString "G" = Just G
fromString _ = Nothing

instance cyclicEnumNoteLetter :: CyclicEnum NoteLetter where
  succ' (NoteLetter n) = NoteLetter n { primitive = succ' n.primitive }
  pred' (NoteLetter n) = NoteLetter n { primitive = pred' n.primitive }

instance enumNoteLetter :: Enum NoteLetter where
  succ n = Just $ succ' n
  pred n = Just $ pred' n

derive instance eqNoteLetter :: Eq NoteLetter
instance ordNoteLetter :: Ord NoteLetter where
  compare (NoteLetter n) (NoteLetter m) = compare n.primitive m.primitive

instance noteLetterPrint :: Print NoteLetter where
  print (NoteLetter letter) =
    if letter.lowercase
    then toLower uppercase
    else uppercase
    where uppercase = print letter.primitive

newtype Transposition = Transposition Int

instance transpositionShow :: Show Transposition where
  show (Transposition i) = if i >= 0 then "+" <> show i else show i

instance transpositionEq :: Eq Transposition where
  eq (Transposition i) (Transposition j) = eq i j

instance transpositionOrd :: Ord Transposition where
  compare (Transposition i) (Transposition j) = compare i j

identityTransposition :: Transposition
identityTransposition = Transposition 0
succTransposition :: Transposition -> Transposition
succTransposition (Transposition i) = Transposition $ i+1
predTransposition :: Transposition -> Transposition
predTransposition (Transposition i) = Transposition $ i-1

instance showLine :: Show TablatureDocumentLine where
  show (TitleLine elems) = "Title: " <> show elems
  show (TablatureLine elems) = "Tab: " <> show elems
  show (TextLine elems) = "Text: " <> show elems
  show (ChordLine elems) = "Chords: " <> show elems
  show (HeaderLine elems) = "Header: " <> show elems

instance showTablatureLineElem :: Show TablatureLineElem where
  show (Prefix string) = string
  show (Tuning spacedNote) = print spacedNote
  show (Timeline string) = string
  show (Fret string) = string
  show (Special string) = string
  show (Suffix string) = string

instance showTextLineElem :: Show TextLineElem where
  show (Text string) = string
  show (Spaces string) = string
  show (TextLineChord chord) = print chord
  show (ChordLegend _) = "legend"

instance showChordLineElem :: Show ChordLineElem where
  show (ChordLineChord chord) = print chord
  show (ChordComment string) = string

instance showChordMod :: Show ChordMod where
  show (ChordMod x) = x.pre <> x.interval <> x.post

instance showHeaderLineElem :: Show HeaderLineElem where
  show (Header string) = string
  show (HeaderSuffix string) = string

instance showTitleLineElem :: Show TitleLineElem where
  show (Title string) = string
  show (TitleOther string) = string

