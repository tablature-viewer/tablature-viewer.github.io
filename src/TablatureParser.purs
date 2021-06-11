module TablatureParser where

import Prelude hiding (between)
import Text.Parsing.StringParser
import Text.Parsing.StringParser.CodePoints
import Text.Parsing.StringParser.Combinators
import Utils

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty as NEL
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (error)

type TabAst = List Line

data Line
  = TitleLine {prefix::String, title::String, suffix::String}
  | TabLine (List TabLineElem)
  | CommentLine String

data TabLineElem
  = Prefix String
  | Suffix String
  | TimeLine String
  | Fret String
  | Special String

instance showLine :: Show Line where
  show (CommentLine string) = "Comment: " <> string
  show (TabLine elems) = "Tab: " <> show elems
  show (TitleLine line) = "Title: " <> line.prefix <> "|" <> line.title <> "|" <> line.suffix

instance showTabLineElem :: Show TabLineElem where
  show (Prefix string) = string
  show (Suffix string) = string
  show (TimeLine string) = string
  show (Fret string) = string
  show (Special string) = string

parseTabAst :: Parser TabAst
parseTabAst = do
  pre <- option Nil parsePreAmble
  -- firstLine <- option Nil (parseTitleLine <#> \result -> result:Nil)
  tab <- parseTab
  -- pure $ pre <> firstLine <> tab
  pure $ pre <> tab
  where
  -- parsePreAmble = many parseCommentLine
  -- parsePreAmble = manyTill parseCommentLine (lookAhead $ (try parseTitleLine <|> try parseTabLine))
  parsePreAmble = manyTill parseCommentLine (lookAhead parseTitleLine)
  parseTab = many parseCommentLine
  -- parseTab = many $ (try parseTabLine) <|> parseCommentLine

parseTitleLine :: Parser Line
parseTitleLine = do
  p <- regex """[^\w\n\r]*"""
  t <- regex """[^\n\r]*\w"""
  s <- regex """[^\n\r]*""" <* parseEndOfLine
  pure $ TitleLine {prefix:p, title:t, suffix:s}

parseTabLine :: Parser Line
parseTabLine = do
  p <- regex """[^|\n\r]*""" <#> \result -> Prefix result
  t <- regex """\|[^|\n\r]+\|""" <#> \result -> TimeLine result
  s <- regex """[^\n\r]*""" <* parseEndOfLine <#> \result -> Suffix result
  pure $ TabLine (p:t:s:Nil)

parseCommentLine :: Parser Line
parseCommentLine = (regex """[^\n\r]+""" <* parseEndOfLine) <|> (parseEndOfLineString *> pure "") <#> \result -> CommentLine result

-- | We are as flexible as possible when it comes to line endings.
-- | Any of the following forms are considered valid: \n \r \n\r eof.
parseEndOfLine :: Parser Unit
parseEndOfLine = parseEndOfLineString *> pure unit <|> eof

parseEndOfLineString :: Parser String
parseEndOfLineString = regex """\n\r?|\r""" 
