module TablatureParser where

import Prelude hiding (between)

import AppState (Chord, ChordLegendElem(..), ChordLineElem(..), ChordMod(..), HeaderLineElem(..), TablatureDocument, TablatureDocumentLine(..), TablatureLineElem(..), TextLineElem(..), TitleLineElem(..), Note, fromString)
import Control.Alt ((<|>))
import Data.Array (elem)
import Data.Either (Either(..))
import Data.List (List(..), fromFoldable, (:))
import Data.Maybe (Maybe(..), fromJust)
import Data.String (drop, singleton)
import Data.String.CodePoints (toCodePointArray)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (Parser, try, unParser)
import Text.Parsing.StringParser.CodePoints (eof, regex, string)
import Text.Parsing.StringParser.Combinators (lookAhead, option, optionMaybe)
import Utils (safeMany, safeManyTill)

-- TODO: Improve the parser code

parseTablatureDocument :: Parser TablatureDocument
parseTablatureDocument = do
  commentLinesBeforeTitle <- option Nil $ (try $ safeManyTill parseTextLine (try $ lookAhead (parseTitleLine <|> parseTablatureLine)))
  title <- option Nil $ (try parseTitleLine) <#> \result -> result:Nil
  body <- safeMany ((try parseTablatureLine) <|> (try parseChordLine) <|> (try parseHeaderLine) <|> (try parseTextLine) <|> parseAnyLine)
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
  tabLine <- try $ lookAhead (regex """\|\|?""") *> safeMany
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
parseChordLine = (safeMany parseChordComment <> (parseChordLineChord <#> \c -> c:Nil) <> safeMany (parseChordLineChord <|> parseChordComment) <* parseEndOfLine) <#> \result -> ChordLine result

parseChordLineChord :: Parser ChordLineElem
parseChordLineChord = (parseChord <#> \chord -> ChordLineChord chord)

parseChord :: Parser Chord
parseChord = do
  rootLetter <- parseRootLetter
  rootMod <- parseRootMod
  chordType <- parseChordType
  mods <- parseChordMods
  maybeBass <- optionMaybe parseChordBass
  assertWordBoundary
  pure $
    { root: { letter: unsafePartial (fromJust rootLetter), mod: rootMod }
    , type: chordType
    , mods: (ChordMod { pre: "", interval:mods, post: "" }):Nil
    , bass: maybeBass
    }
  where
  parseRootLetter = regex """(?<!\S)[A-G]""" <#> fromString
  parseRootMod = regex """[#b]*"""
  parseChordType = regex """(ø|Δ| ?Major| ?major|Maj|maj|Ma| ?Minor| ?minor|Min|min|M|m|[-]|[+]|o)?"""
  parseChordMods = regex """((sus[24]?)|\(?(o|no|add|dim|dom|augm(?![a-zA-Z])|aug|maj|Maj|M|Δ)?([2-9]|10|11|12|13)?(b|#|[+]|[-])?\)?)*"""

parseChordBass :: Parser Note
parseChordBass = do
  _ <- string "/"
  bassLetter <- parseChordBassLetter
  bassMod <- parseBassMod
  pure $ { letter: unsafePartial (fromJust bassLetter), mod: bassMod }
  where
  parseChordBassLetter = regex """([A-G])""" <#> fromString
  parseBassMod = regex """[#b]*"""

-- A chord comment is a non chord string that is either a series of dots or a series of spaces or a parenthesized expression.
parseChordComment :: Parser ChordLineElem
parseChordComment = regex """[^\S\n\r]*(\([^\n\r()]*\)|\.\.+| +)[^\S\n\r]*""" <#> \result -> ChordComment result

parseTextLine :: Parser TablatureDocumentLine
parseTextLine = safeManyTill (parseSpaces <|> try (parseChord <#> \chord  -> TextLineChord chord) <|> parseChordLegend <|> parseWord) parseEndOfLine
  <#> \result -> TextLine result

parseSpaces :: Parser TextLineElem
parseSpaces = regex """[^\S\n\r]+""" <#> \result -> Spaces result

parseWord :: Parser TextLineElem
parseWord = regex """(?<!\S)\S+(?!\S)""" <#> \result -> Text result

assertWordBoundary :: Parser Unit
assertWordBoundary = eof <|> lookAhead (regex """\s""") *> pure unit

parseChordLegend :: Parser TextLineElem
parseChordLegend = regex """(?<!\S)[\dxX]{6}(?!\S)""" <#> \result -> ChordLegend $ fromFoldable $ map
  (\c -> if elem c (toCodePointArray "1234567890") then ChordFret $ singleton c else ChordSpecial $ singleton c) $ toCodePointArray result

-- This is a backup in case the other parsers fail
parseAnyLine :: Parser TablatureDocumentLine
parseAnyLine = regex """[^\n\r]*""" <* parseEndOfLine <#> \result -> TextLine ((Text result):Nil)

-- | We are as flexible as possible when it comes to line endings.
-- | Any of the following forms are considered valid: \n \r \n\r eof
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
