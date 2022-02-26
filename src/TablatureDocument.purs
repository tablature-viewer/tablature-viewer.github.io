module TablatureDocument where

import Prelude

import Data.Enum (class Enum, pred, succ)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.Lens (Prism', Lens', prism')
import Data.Lens.Barlow (barlow, key)
import Data.List (List, findIndex, (!!))
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (toLower)
import Data.String.Utils (repeat)
import Partial.Unsafe (unsafePartial)
import Utils (class CyclicEnum, class Print, print)

type TablatureDocument = List TablatureDocumentLine

data TablatureDocumentLine
  = TitleLine (List TitleLineElem) -- Title of whole document
  | HeaderLine (List HeaderLineElem) -- Header per section of document
  | TablatureLine (List TablatureLineElem)
  | ChordLine (List ChordLineElem)
  | TextLine (List TextLineElem)
derive instance Generic TablatureDocumentLine _

-- TOD: change to lens
getTitle :: TablatureDocument -> Maybe String
getTitle tablatureDocument = 
  case findElement isTitleLine tablatureDocument of
    Just (TitleLine line) ->
      case findElement isTitle line of
        Just (Title title) -> Just title
        _ -> Nothing
    _ -> Nothing
  where
  isTitleLine (TitleLine _) = true
  isTitleLine _ = false
  isTitle (Title _) = true
  isTitle _ = false
  findElement :: forall a. (a -> Boolean) -> List a -> Maybe a
  findElement p l =
    case findIndex p l of
      Nothing -> Nothing
      Just index -> case l !! index of
        Nothing -> Nothing
        Just elem -> Just elem

_TitleLine :: Prism' TablatureDocumentLine (List TitleLineElem)
_TitleLine = barlow (key :: _ "%TitleLine")

_TablatureLine :: Prism' TablatureDocumentLine (List TablatureLineElem)
_TablatureLine = barlow (key :: _ "%TablatureLine")

_ChordLine :: Prism' TablatureDocumentLine (List ChordLineElem)
_ChordLine = barlow (key :: _ "%ChordLine")

_TextLine :: Prism' TablatureDocumentLine (List TextLineElem)
_TextLine = barlow (key :: _ "%TextLine")

data TitleLineElem
  = Title String
  | TitleOther String

-- _Title :: Prism' (List TitleLineElem) String
_Title = barlow (key :: _ "+%Title")

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
derive instance Newtype (Spaced a) _

_elem :: forall a . Lens' (Spaced a) a
_elem = barlow (key :: _ "!.elem")
_spaceSuffix :: forall a . Lens' (Spaced a) Int
_spaceSuffix = barlow (key :: _ "!.spaceSuffix")

instance (Print a) => Print (Spaced a) where
  print (Spaced x) = print x.elem <> (fromMaybe "" $ repeat x.spaceSuffix " ")

newtype Chord = Chord
  { root :: Note
  , type :: String
  , mods :: List ChordMod
  , bass :: Maybe Note
  }
derive instance Newtype Chord _

_root :: Lens' Chord Note
_root = barlow (key :: _ "!.root")
_type :: Lens' Chord String
_type = barlow (key :: _ "!.type")
_bass :: Lens' Chord (Maybe Note)
_bass = barlow (key :: _ "!.bass")
_mods :: Lens' Chord (List ChordMod)
_mods = barlow (key :: _ "!.mods")

derive instance Eq Chord

instance Print Chord where
  print (Chord chord) =
    (chord.root # print)
    <> chord.type
    <> (foldr (<>) "" (map (\(ChordMod mod) -> mod.pre <> mod.interval <> mod.post) chord.mods))
    <> (fromMaybe "" $ chord.bass <#> print)

instance Print Note where
  print (Note note) = print note.letter <> note.mod

newtype ChordMod = ChordMod
  { pre :: String
  , interval :: String
  , post :: String
  }
instance Print ChordMod where
  print (ChordMod mod) = mod.pre <> mod.interval <> mod.post

derive instance Eq ChordMod

newtype Note = Note
  { letter :: NoteLetter
  , mod :: String
  }
derive instance Newtype Note _

_letter :: Lens' Note NoteLetter
_letter = barlow (key :: _ "!.letter")
_mod :: Lens' Note String
_mod = barlow (key :: _ "!.mod")

derive instance Eq Note

newtype NoteLetter = NoteLetter
  { primitive :: NoteLetterPrimitive
  , lowercase :: Boolean
  }
derive instance Newtype NoteLetter _

_primitive :: Lens' Note NoteLetterPrimitive
_primitive = barlow (key :: _ "!.letter!.primitive")
_lowercase :: Lens' Note Boolean
_lowercase = barlow (key :: _ "!.letter!.lowercase")

data NoteLetterPrimitive = A | B | C | D | E | F | G

derive instance Eq NoteLetterPrimitive
derive instance Ord NoteLetterPrimitive
derive instance Generic NoteLetterPrimitive _
instance Enum NoteLetterPrimitive where
  succ G = Just A
  succ x = genericSucc x
  pred A = Just G
  pred x = genericPred x

instance CyclicEnum NoteLetterPrimitive where
  succ' x = unsafePartial $ fromJust $ succ x
  pred' x = unsafePartial $ fromJust $ pred x

instance Print NoteLetterPrimitive where
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

derive instance Eq NoteLetter
instance Ord NoteLetter where
  compare (NoteLetter n) (NoteLetter m) = compare n.primitive m.primitive

instance Print NoteLetter where
  print (NoteLetter letter) =
    if letter.lowercase
    then toLower uppercase
    else uppercase
    where uppercase = print letter.primitive


newtype Transposition = Transposition Int

instance Show Transposition where
  show (Transposition i) = if i >= 0 then "+" <> show i else show i

instance Eq Transposition where
  eq (Transposition i) (Transposition j) = eq i j

instance Ord Transposition where
  compare (Transposition i) (Transposition j) = compare i j

identityTransposition :: Transposition
identityTransposition = Transposition 0
succTransposition :: Transposition -> Transposition
succTransposition (Transposition i) = Transposition $ i+1
predTransposition :: Transposition -> Transposition
predTransposition (Transposition i) = Transposition $ i-1

instance Show TablatureDocumentLine where
  show (TitleLine elems) = "Title: " <> show elems
  show (TablatureLine elems) = "Tab: " <> show elems
  show (TextLine elems) = "Text: " <> show elems
  show (ChordLine elems) = "Chords: " <> show elems
  show (HeaderLine elems) = "Header: " <> show elems
 
instance Show TablatureLineElem where
  show (Prefix string) = string
  show (Tuning spacedNote) = print spacedNote
  show (Timeline string) = string
  show (Fret string) = string
  show (Special string) = string
  show (Suffix string) = string
 
instance Show TextLineElem where
  show (Text string) = string
  show (Spaces string) = string
  show (TextLineChord chord) = print chord
  show (ChordLegend _) = "legend"
 
instance Show ChordLineElem where
  show (ChordLineChord chord) = print chord
  show (ChordComment string) = string
 
instance Show ChordMod where
  show (ChordMod x) = x.pre <> x.interval <> x.post
 
instance Show HeaderLineElem where
  show (Header string) = string
  show (HeaderSuffix string) = string
 
instance Show TitleLineElem where
  show (Title string) = string
  show (TitleOther string) = string
