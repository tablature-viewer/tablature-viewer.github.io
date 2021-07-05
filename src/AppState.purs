module AppState where

import Prelude

import Data.List (List)
import Data.Maybe (Maybe)

data Mode = ViewMode | EditMode

type State =
  { mode :: Mode
  , loading :: Boolean
  , tablatureText :: String
  , tablatureTitle :: String
  , tablatureDocument :: Maybe TablatureDocument
  -- Store the scrollTop in the state before actions so we can restore the expected scrollTop when switching views
  , scrollTop :: Number
  , dozenalizationEnabled :: Boolean
  }

type TablatureDocument = List TablatureDocumentLine

data TablatureDocumentLine
  = TitleLine {prefix::String, title::String, suffix::String}
  | TablatureLine (List TablatureLineElem)
  | HeaderLine (List HeaderLineElem)
  | ChordLine (List ChordLineElem)
  | TextLine (List TextLineElem)

data TablatureLineElem
  = Prefix String
  | Suffix String
  | Timeline String
  | Fret String
  | Special String

data TextLineElem
  = Text String

data ChordLineElem
  = Chord String

data HeaderLineElem
  = Header String

instance showLine :: Show TablatureDocumentLine where
  show (TitleLine line) = "Title: " <> line.prefix <> "|" <> line.title <> "|" <> line.suffix
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

instance showChordLineElem :: Show ChordLineElem where
  show (Chord string) = string

instance showHeaderLineElem :: Show HeaderLineElem where
  show (Header string) = string


data Action = Initialize | ToggleEditMode | ToggleDozenalization | CopyShortUrl

instance showMode :: Show Mode where
  show ViewMode = "View Mode"
  show EditMode = "Edit Mode"
