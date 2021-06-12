module TablatureParser where

import Prelude hiding (between)
import Text.Parsing.StringParser
import Text.Parsing.StringParser.CodePoints
import Text.Parsing.StringParser.Combinators
import Utils

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String (drop)
import Effect.Console (error)
import Effect.Unsafe (unsafePerformEffect)

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

parseTablatureDocument :: Parser TablatureDocument
parseTablatureDocument = do
  header <- option Nil $ (try parseTitleLine) <#> \result -> result:Nil
  body <- many $ (try parseTablatureLine) <|> parseCommentLine
  pure $ header <> body

parseTitleLine :: Parser TablatureDocumentLine
parseTitleLine = do
  p <- regex """[^\w\n\r]*"""
  t <- regex """[^\n\r]*."""
  s <- regex """[^\n\r]*""" <* parseEndOfLine
  pure $ TitleLine {prefix:p, title:t, suffix:s}

parseTablatureLine :: Parser TablatureDocumentLine
parseTablatureLine = do
  p <- regex """[^|\n\r]*""" <#> \result -> Prefix result
  t <- lookAhead (regex """\|[^\n\r]+\|""") *> many
    ((regex """(\||-)+""" <#> \result -> Timeline result) <|>
    (regex """\d+""" <#> \result -> Fret result) <|>
    (regex """[^\r\n\d|-]+""" <#> \result -> Special result))
  s <- regex """[^\n\r]*""" <* parseEndOfLine <#> \result -> Suffix result
  pure $ TablatureLine (p:Nil <> t <> s:Nil)

parseCommentLine :: Parser TablatureDocumentLine
parseCommentLine = (regex """[^\n\r]+""" <* parseEndOfLine) <|> (parseEndOfLineString *> pure "") <#> \result -> CommentLine result

-- | We are as flexible as possible when it comes to line endings.
-- | Any of the following forms are considered valid: \n \r \n\r eof.
parseEndOfLine :: Parser Unit
parseEndOfLine = parseEndOfLineString *> pure unit <|> eof

parseEndOfLineString :: Parser String
parseEndOfLineString = regex """\n\r?|\r""" 

tryRunParser :: forall a. Show a => Parser a -> String -> Maybe a
tryRunParser parser inputString = 
  case unParser (parser <* eof) { str: inputString, pos: 0 } of
    Left rec -> error msg # unsafePerformEffect # \_ -> Nothing
      where
      msg = "Position: " <> show rec.pos
        <> "\nError: " <> show rec.error
        <> "\nIn input string: " <> inputString
        <> "\nWith unparsed suffix: " <> (show $ drop rec.pos inputString)
    Right rec -> do
      pure rec.result
