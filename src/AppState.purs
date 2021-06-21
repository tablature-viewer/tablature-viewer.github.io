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
  | TablatureLine (List TablatureElem)
  | CommentLine String

data TablatureElem
  = Prefix String
  | Suffix String
  | Timeline String
  | Fret String
  | Special String


instance showLine :: Show TablatureDocumentLine where
  show (TitleLine line) = "Title: " <> line.prefix <> "|" <> line.title <> "|" <> line.suffix
  show (TablatureLine elems) = "Tab: " <> show elems
  show (CommentLine string) = "Comment: " <> string

instance showTablatureElem :: Show TablatureElem where
  show (Prefix string) = string
  show (Suffix string) = string
  show (Timeline string) = string
  show (Fret string) = string
  show (Special string) = string


data Action = Initialize | ToggleEditMode | ToggleDozenalization | CopyShortUrl

instance showMode :: Show Mode where
  show ViewMode = "View Mode"
  show EditMode = "Edit Mode"
