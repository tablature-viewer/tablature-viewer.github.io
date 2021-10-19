module AppState where

import Prelude

import Data.Enum (class Enum, pred, succ)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Foldable (foldr)
import Data.List (List)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Show.Generic (genericShow)
import Effect.Timer (IntervalId)
import Partial.Unsafe (unsafePartial)
import Utils (class CyclicEnum)

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

data TitleLineElem
  = Title String
  | TitleOther String

data HeaderLineElem
  = Header String
  | HeaderSuffix String

data TablatureLineElem
  = Prefix String
  | Suffix String
  | Timeline String
  | Fret String
  | Special String

data TextLineElem
  = Text String
  | Spaces String
  | TextLineChord Chord
  | ChordLegend (List ChordLegendElem)

data ChordLegendElem
  = ChordFret String
  | ChordSpecial String

data ChordLineElem
  = ChordLineChord Chord
  | ChordComment String

type Chord =
  { root :: Note
  , type :: String
  , mods :: List ChordMod
  , bass :: Maybe Note
  -- The number of spaces after a chord. This is part of the chord so that it can be expanded and shrunk easily when rewriting chords
  -- TODO: lift this member into a wrapper type
  , spaceSuffix :: Int
  }

getPlainChordString :: Chord -> String
getPlainChordString chord =
   (chord.root # getPlainNoteString)
   <> chord.type
   <> (foldr (<>) "" (map (\(ChordMod mod) -> mod.pre <> mod.interval <> mod.post) chord.mods))
   <> (fromMaybe "" $ chord.bass <#> getPlainNoteString)

getPlainNoteString :: Note -> String
getPlainNoteString note = show note.letter <> note.mod

newtype ChordMod = ChordMod
  { pre :: String
  , interval :: String
  , post :: String
  }

type Foo =
  { a :: String
  , b :: String
  }

type Note =
  { letter :: NoteLetter
  , mod :: String
  }

data NoteLetter = A | B | C | D | E | F | G

derive instance eqNoteLetter :: Eq NoteLetter

derive instance ordNoteLetter :: Ord NoteLetter
derive instance genericNoteLetter :: Generic NoteLetter _
instance noteLetterShow :: Show NoteLetter where
  show = genericShow
instance enumNoteLetter :: Enum NoteLetter where
  succ G = Just A
  succ x = genericSucc x
  pred A = Just G
  pred x = genericPred x

fromString :: String -> Maybe NoteLetter
fromString "A" = Just A
fromString "B" = Just B
fromString "C" = Just C
fromString "D" = Just D
fromString "E" = Just E
fromString "F" = Just F
fromString "G" = Just G
fromString _ = Nothing

instance cyclicEnumNoteLetter :: CyclicEnum NoteLetter where
  succ' x = unsafePartial $ fromJust $ succ x
  pred' x = unsafePartial $ fromJust $ pred x

-- derive instance genericNote :: Generic Note

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
  show (Suffix string) = string
  show (Timeline string) = string
  show (Fret string) = string
  show (Special string) = string

instance showTextLineElem :: Show TextLineElem where
  show (Text string) = string
  show (Spaces string) = string
  show (TextLineChord chord) = show chord
  show (ChordLegend _) = "legend"

instance showChordLineElem :: Show ChordLineElem where
  show (ChordLineChord chord) = show chord
  show (ChordComment string) = string

instance showChordMod :: Show ChordMod where
  show (ChordMod x) = x.pre <> x.interval <> x.post

instance showHeaderLineElem :: Show HeaderLineElem where
  show (Header string) = string
  show (HeaderSuffix string) = string

instance showTitleLineElem :: Show TitleLineElem where
  show (Title string) = string
  show (TitleOther string) = string

