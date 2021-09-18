module TablatureParser where

import Prelude hiding (between)

import AppState (ChordLineElem(..), HeaderLineElem(..), TablatureDocument, TablatureDocumentLine(..), TablatureLineElem(..), TextLineElem(..), TitleLineElem(..))
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String (drop)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Text.Parsing.StringParser (Parser, try, unParser)
import Text.Parsing.StringParser.CodePoints (eof, regex)
import Text.Parsing.StringParser.Combinators (lookAhead, many, manyTill, option)


parseTablatureDocument :: Parser TablatureDocument
parseTablatureDocument = do
  commentLinesBeforeTitle <- option Nil $ (try $ manyTill parseTextLine (try $ lookAhead (parseTitleLine <|> parseTablatureLine)))
  title <- option Nil $ (try parseTitleLine) <#> \result -> result:Nil
  body <- many $ (try parseTablatureLine) <|> (try parseChordLine) <|> (try parseHeaderLine) <|> parseTextLine
  pure $ commentLinesBeforeTitle <> title <> body

parseTitleLine :: Parser TablatureDocumentLine
parseTitleLine = do
  prefix <- regex """[^\w\n\r]*"""
  title <- regex """[^\n\r]*[\w)!?"']"""
  suffix <- regex """[^\n\r]*""" <* parseEndOfLine
  pure $ TitleLine $ TitleOther prefix : Title title : TitleOther suffix : Nil

parseTablatureLine :: Parser TablatureDocumentLine
parseTablatureLine = do
  prefix <- regex """[^|\n\r]*""" <#> \result -> Prefix result
  tabLine <- try $ lookAhead (regex """\|\|?""") *> many
    (
      -- We allow normal dashes - and em dashes —
      (regex """(([\-—](?!\|)|([\-—]?\|\|?(?=[^\s\-—|]*[\-—|]))))+""" <#> \result -> Timeline result) <|>
      (regex ("""[\d↊↋]+""") <#> \result -> Fret result) <|>
      (regex ("""[^\s|\-—\d↊↋]+""") <#> \result -> Special result)
    )
  tabLineClose <- regex """[\-—]?\|?\|?""" <#> \result -> Timeline result
  suffix <- regex """[^\n\r]*""" <* parseEndOfLine <#> \result -> Suffix result
  pure $ TablatureLine (prefix:Nil <> tabLine <> tabLineClose:Nil <> suffix:Nil)

parseHeaderLine :: Parser TablatureDocumentLine
parseHeaderLine = do
  header <- regex """[ \t]*\[[^\n\r]+\]"""
  suffix <- regex """[^\r\n]*""" <* parseEndOfLine
  pure $ HeaderLine ((Header header):(HeaderSuffix suffix):Nil)

parseChordLine :: Parser TablatureDocumentLine
parseChordLine = (many parseChordComment <> (parseChord <#> \c -> c:Nil) <> many (parseChord <|> parseChordComment <|> parseChordLegend) <* parseEndOfLine) <#> \result -> ChordLine result

parseChord :: Parser ChordLineElem
parseChord = do
  root <- parseChordRoot
  rootMod <- parseChordRootMod
  chordType <- parseChordType
  mods <- parseChordMods
  bass <- parseChordBass
  bassMod <- parseChordRootMod
  pure $ Chord { root : root
  , rootMod : rootMod
  , type : chordType
  , mods : mods
  , bass : bass
  , bassMod : bassMod
  }
  where
  parseChordRoot = regex """[A-G]"""
  parseChordRootMod = regex """[#b]*"""
  parseChordType = regex """(ø|Δ|major|Maj|Ma|maj|Min|minor|min|M|m|[-]|dim|sus|dom|aug|[+]|o)?"""
  parseChordMods = regex """(\(?(b|#|[+]|o|no|add|dim|aug|maj|Maj|M|Δ)?([2-9]|10|11|12|13)?\)?)*"""
  parseChordBass = regex """(/[A-G])?"""

parseChordLegend :: Parser ChordLineElem
parseChordLegend = regex """[\dxX]{6}""" <#> \result -> ChordLegend result

-- A chord comment is a non chord string that is either a series of dots or a series of spaces or a parenthesized expression.
parseChordComment :: Parser ChordLineElem
parseChordComment = regex """[ \t]*(\([^\n\r()]*\)|\.\.+| +)[ \t]*""" <#> \result -> ChordComment result

parseTextLine :: Parser TablatureDocumentLine
parseTextLine = (regex """[^\n\r]+""" <* parseEndOfLine) <|> (parseEndOfLineString *> pure "") <#> \result -> TextLine ((Text result):Nil)

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
