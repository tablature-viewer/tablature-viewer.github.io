module TablatureParser where

import Prelude hiding (between)

import AppState (TablatureDocument, TablatureDocumentLine(..), TablatureElem(..))
import Text.Parsing.StringParser (Parser, try, unParser)
import Text.Parsing.StringParser.CodePoints (eof, regex, string)
import Text.Parsing.StringParser.Combinators (lookAhead, many, manyTill, option)

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (drop)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)


parseTablatureDocument :: Parser TablatureDocument
parseTablatureDocument = do
  commentLinesBeforeTitle <- option Nil $ (try $ manyTill parseCommentLine (try $ lookAhead (parseTitleLine <|> parseTablatureLine)))
  title <- option Nil $ (try parseTitleLine) <#> \result -> result:Nil
  body <- many $ (try parseTablatureLine) <|> parseCommentLine
  pure $ commentLinesBeforeTitle <> title <> body

parseTitleLine :: Parser TablatureDocumentLine
parseTitleLine = do
  p <- regex """[^\w\n\r]*"""
  t <- regex """[^\n\r]*\w"""
  s <- regex """[^\n\r]*""" <* parseEndOfLine
  pure $ TitleLine {prefix:p, title:t, suffix:s}

parseTablatureLine :: Parser TablatureDocumentLine
parseTablatureLine = do
  p <- regex """[^|\n\r]*""" <#> \result -> Prefix result
  t <- try $ lookAhead (regex """\|\|?""") *> many
    (
      (regex """((-(?!\|)|(-?\|\|?(?=[^\r\n\-|]*[\-|]))))+""" <#> \result -> Timeline result) <|>
      (regex """\d+""" <#> \result -> Fret $ fromMaybe 0 $ fromString result) <|>
      (regex """[^\r\n\d|\-]+""" <#> \result -> Special result)
    )
  tClose <- regex """-?\|?\|?""" <#> \result -> Timeline result
  s <- regex """[^\n\r]*""" <* parseEndOfLine <#> \result -> Suffix result
  pure $ TablatureLine (p:Nil <> t <> tClose:Nil <> s:Nil)

parseCommentLine :: Parser TablatureDocumentLine
parseCommentLine = (regex """[^\n\r]+""" <* parseEndOfLine) <|> (parseEndOfLineString *> pure "") <#> \result -> CommentLine result

-- | We are as flexible as possible when it comes to line endings.
-- | Any of the following forms are considered valid: \n \r \n\r eof.
parseEndOfLine :: Parser Unit
parseEndOfLine = parseEndOfLineString *> pure unit <|> eof

parseEndOfLineString :: Parser String
parseEndOfLineString = regex """\n\r?|\r""" 

tryParseTablature :: String -> Maybe (List TablatureDocumentLine)
tryParseTablature inputString = tryRunParser parseTablatureDocument inputString

tryRunParser :: forall a. Show a => Parser a -> String -> Maybe a
tryRunParser parser inputString = 
  case unParser (parser <* eof) { str: inputString, pos: 0 } of
    Left rec -> Console.error msg # unsafePerformEffect # \_ -> Nothing
      where
      msg = "Position: " <> show rec.pos
        <> "\nError: " <> show rec.error
        <> "\nIn input string: " <> inputString
        <> "\nWith unparsed suffix: " <> (show $ drop rec.pos inputString)
    Right rec -> do
      pure rec.result
