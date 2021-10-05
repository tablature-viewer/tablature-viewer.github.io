module TablatureParser where

import Prelude hiding (between)

import AppState (Chord, ChordLegendElem(..), ChordLineElem(..), HeaderLineElem(..), TablatureDocument, TablatureDocumentLine(..), TablatureLineElem(..), TextLineElem(..), TitleLineElem(..), ChordMod(..))
import Control.Alt ((<|>))
import Data.Array (elem)
import Data.Either (Either(..))
import Data.List (List(..), fromFoldable, (:))
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe(..))
import Data.String (drop, singleton)
import Data.String.CodePoints (codePointAt, codePointFromChar, length, toCodePointArray)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Text.Parsing.StringParser (Parser, try, unParser)
import Text.Parsing.StringParser.CodePoints (eof, regex)
import Text.Parsing.StringParser.Combinators (lookAhead, many, many1Till, manyTill, option)

-- NOTES
-- many p will get stuck in a loop if p possibly doesn't consume any input but still succeeds
-- many (many p) will get stuck for any p
-- parseEndOfLine doesn't consume input at the end of the file but still succeeds

-- TODO: Improve the parser code
-- TODO: Prove that the parser can never get stuck into a loop

parseTablatureDocument :: Parser TablatureDocument
parseTablatureDocument = do
  commentLinesBeforeTitle <- option Nil $ (try $ manyTill parseTextLine (try $ lookAhead (parseTitleLine <|> parseTablatureLine)))
  title <- option Nil $ (try parseTitleLine) <#> \result -> result:Nil
  body <- many ((try parseTablatureLine) <|> (try parseChordLine) <|> (try parseHeaderLine) <|> (try parseTextLine) <|> parseAnyLine)
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
  header <- regex """[^\S\n\r]*\[[^\n\r]+\]"""
  suffix <- regex """[^\r\n]*""" <* parseEndOfLine
  pure $ HeaderLine ((Header header):(HeaderSuffix suffix):Nil)

parseChordLine :: Parser TablatureDocumentLine
parseChordLine = (many parseChordComment <> (parseChordLineChord <#> \c -> c:Nil) <> many (parseChordLineChord <|> parseChordComment) <* parseEndOfLine) <#> \result -> ChordLine result

parseChordLineChord :: Parser ChordLineElem
parseChordLineChord = (parseChord <#> \chord -> ChordLineChord chord)

parseChord :: Parser Chord
parseChord = do
  rootLetter <- parseChordRoot
  rootMod <- parseChordRootMod
  chordType <- parseChordType
  mods <- parseChordMods
  bassLetter <- parseChordBass
  bassMod <- parseBassMod
  pure $
    { root: { letter: rootLetter, mod: rootMod }
    , type: chordType
    , mods: (ChordMod { pre: "", interval:mods, post: "" }):Nil
    , bass: { letter: bassLetter, mod: bassMod }
    }
  where
  parseChordRoot = regex """(?<!\S)[A-G]"""
  parseChordRootMod = regex """[#b]*"""
  parseChordType = regex """(ø|Δ| ?Major| ?major|Maj|maj|Ma| ?Minor| ?minor|Min|min|M|m|[-]|[+]|o)?"""
  parseChordMods = regex """((sus[24]?)|\(?(o|no|add|dim|dom|augm(?![a-zA-Z])|aug|maj|Maj|M|Δ)?([2-9]|10|11|12|13)?(b|#|[+]|[-])?\)?)*"""
  parseChordBass = regex """(/[A-G])?"""
  parseBassMod = regex """[#b]*(?!\S)"""

-- A chord comment is a non chord string that is either a series of dots or a series of spaces or a parenthesized expression.
parseChordComment :: Parser ChordLineElem
parseChordComment = regex """[^\S\n\r]*(\([^\n\r()]*\)|\.\.+| +)[^\S\n\r]*""" <#> \result -> ChordComment result

parseTextLine :: Parser TablatureDocumentLine
parseTextLine = many1Till (parseSpaces <|> try (parseChord <#> \chord  -> TextLineChord chord) <|> parseChordLegend <|> parseWord) parseEndOfLine
  <#> \result -> TextLine $ toList result

parseSpaces :: Parser TextLineElem
parseSpaces = regex """[^\S\n\r]+""" <#> \result -> Spaces result

parseWord :: Parser TextLineElem
parseWord = regex """(?<!\S)\S+(?!\S)""" <#> \result -> Text result

parseChordLegend :: Parser TextLineElem
parseChordLegend = regex """(?<!\S)[\dxX]{6}(?!\S)""" <#> \result -> ChordLegend $ fromFoldable $ map
  (\c -> if elem c (toCodePointArray "1234567890") then ChordFret $ singleton c else ChordSpecial $ singleton c) $ toCodePointArray result

-- This is a backup in case the other parsers fail
parseAnyLine :: Parser TablatureDocumentLine
parseAnyLine = (regex """[^\n\r]+""" <* parseEndOfLine) <|> (parseEndOfLine *> pure "") <#> \result -> TextLine ((Text result):Nil)

-- | We are as flexible as possible when it comes to line endings.
-- | Any of the following forms are considered valid: \n \r \n\r
parseEndOfLine :: Parser Unit
parseEndOfLine = regex """\n\r?|\r""" *> pure unit

tryParseTablature :: String -> Maybe (List TablatureDocumentLine)
tryParseTablature inputString =
  if endsWithNewLine inputString
  then tryRunParser parseTablatureDocument inputString
  -- Add newline after each line to make parsing a lot easier and fail-safe (no more zero width eof leading to infinite loops)
  else tryRunParser parseTablatureDocument (inputString <> "\n")

endsWithNewLine :: String -> Boolean
endsWithNewLine string =
  lastChar == Just (codePointFromChar '\n') || lastChar == Just (codePointFromChar '\r')
  where lastChar = codePointAt (length string - 1) string

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
