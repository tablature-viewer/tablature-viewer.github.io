module TablatureParser where

import Prelude hiding (between)

import TablatureDocument (Chord(..), ChordLegendElem(..), ChordLineElem(..), ChordMod(..), HeaderLineElem(..), Note(..), NoteLetter(..), Spaced(..), TablatureDocument, TablatureDocumentLine(..), TablatureLineElem(..), TextLineElem(..), TitleLineElem(..), fromString)
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String (drop, length, toLower, toUpper)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (Parser, try, unParser)
import Text.Parsing.StringParser.CodePoints (eof, regex, string)
import Text.Parsing.StringParser.Combinators (lookAhead, option, optionMaybe)
import Utils (safeMany, safeManyTill)

-- TODO: Improve the parser code
-- TODO: Improve parser performance

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
  prefix <- safeManyTill (regex """[^|\n\r]""") (lookAhead (optionMaybe parseSpacedNote *> string "|")) <#> \result -> Prefix (foldr (<>) "" result)
  maybeTuning <- optionMaybe $ parseSpacedNote <#> \result -> Tuning result
  tabLine <- try $ lookAhead (regex """\|\|?""") *> safeMany
    (
      -- We allow normal dashes - and em dashes —
      (regex """(([\-—](?!\|)|([\-—]?\|\|?(?=[^\s\-—|]*[\-—|]))))+""" <#> \result -> Timeline result) <|>
      (regex ("""[\d↊↋]+""") <#> \result -> Fret result) <|>
      (regex ("""[^\s|\-—\d↊↋]+""") <#> \result -> Special result)
    )
  tabLineClose <- regex """[\-—]?\|?\|?""" <#> \result -> Timeline result
  suffix <- regex """[^\n\r]*""" <* parseEndOfLine <#> \result -> Suffix result
  pure $ TablatureLine (prefix:Nil <> (getTuning maybeTuning) <> tabLine <> tabLineClose:Nil <> suffix:Nil)
  where
  getTuning = case _ of
    Nothing -> Nil
    Just t -> t:Nil

parseHeaderLine :: Parser TablatureDocumentLine
parseHeaderLine = do
  header <- regex """[^\S\n\r]*\[[^\n\r]+\]"""
  suffix <- regex """[^\r\n]*""" <* parseEndOfLine
  pure $ HeaderLine ((Header header):(HeaderSuffix suffix):Nil)

parseChordLine :: Parser TablatureDocumentLine
parseChordLine = (safeMany parseChordComment <> (parseChordLineChord <#> \c -> c:Nil) <> safeMany (parseChordLineChord <|> parseChordComment) <* parseEndOfLine) <#> \result -> ChordLine result

parseChordLineChord :: Parser ChordLineElem
parseChordLineChord = (parseChord <#> \chord -> ChordLineChord chord)

parseChord :: Parser (Spaced Chord)
parseChord = do
  root <- parseNote
  chordType <- parseChordType
  mods <- parseChordMods
  maybeBass <- optionMaybe (string "/" *> parseNote)
  assertEndChordBoundary
  spaceSuffix <- parseSpaces
  pure $ Spaced
    { elem: Chord
      { root: root
      , type: chordType
      , mods: (ChordMod { pre: "", interval:mods, post: "" }):Nil
      , bass: maybeBass
      }
    , spaceSuffix: length spaceSuffix
    }
  where
  parseChordType = regex """(ø|Δ| ?Major| ?major|Maj|maj|Ma| ?Minor| ?minor|Min|min|M|m|[-]|[+]|o)?"""
  parseChordMods = regex """((sus[24]?)|\(?(o|no|add|dim|dom|augm(?![a-zA-Z])|aug|maj|Maj|M|Δ)?([2-9]|10|11|12|13)?(b|#|[+]|[-])?\)?)*"""

parseNote :: Parser Note
parseNote = do
  rootLetter <- regex """[A-Ga-g]"""
  rootMod <- regex """[#b]*"""
  pure $ Note { letter: NoteLetter $ { primitive: getPrimitive rootLetter, lowercase: lowercase rootLetter }, mod: rootMod }
  where
  getPrimitive rootLetter = unsafePartial (fromJust (fromString (toUpper rootLetter)))
  lowercase letter = toLower letter == letter

parseSpacedNote :: Parser (Spaced Note)
parseSpacedNote = do
  note <- parseNote
  spaceSuffix <- parseSpaces
  pure $ Spaced { elem: note, spaceSuffix: length spaceSuffix }

-- A chord comment is a non chord string that is either a series of dots, a series of spaces, a xN expression or a parenthesized expression.
parseChordComment :: Parser ChordLineElem
parseChordComment = regex """[^\S\n\r]*(\([^\n\r()]*\)|\.\.+|[^\S\n\r]+|x\d+)[^\S\n\r]*""" <#> \result -> ChordComment result

parseTextLine :: Parser TablatureDocumentLine
parseTextLine = safeManyTill (parseTextLineSpace <|> try (parseChord <#> \chord  -> TextLineChord chord) <|> parseChordLegend <|> parseWord) parseEndOfLine
  <#> \result -> TextLine result

parseTextLineSpace :: Parser TextLineElem
parseTextLineSpace = regex """[^\S\n\r]+""" <#> \result -> Spaces result

parseSpaces :: Parser String
parseSpaces = regex """[ ]*"""

parseWord :: Parser TextLineElem
parseWord = regex """(?<!\S)\S+(?!\S)""" <#> \result -> Text result

assertEndChordBoundary :: Parser Unit
assertEndChordBoundary = eof <|> lookAhead (regex """\s""") *> pure unit

parseChordLegend :: Parser TextLineElem
parseChordLegend = lookAhead (regex """(?<!\S)(([\dxX↊↋]{1,2}[-]*){3,30})(?!\S)""")
  *> safeMany ((regex """[1234567890↊↋]""" <#> ChordFret) <|> (regex """[xX-]""" <#> ChordSpecial)) <#> ChordLegend 

-- This is a backup in case the other parsers fail
parseAnyLine :: Parser TablatureDocumentLine
parseAnyLine = regex """[^\n\r]*""" <* parseEndOfLine <#> \result -> TextLine ((Text result):Nil)

-- | We are as flexible as possible when it comes to line endings.
-- | Any of the following forms are considered valid: \n \r \n\r eof
parseEndOfLine :: Parser Unit
parseEndOfLine = parseEndOfLineString *> pure unit <|> eof

parseEndOfLineString :: Parser String
parseEndOfLineString = regex """\n\r?|\r"""

tryParseTablature :: String -> Maybe TablatureDocument
tryParseTablature inputString = tryRunParser parseTablatureDocument inputString

parseTablature :: String -> TablatureDocument
parseTablature text = tryParseTablature text # fromMaybe Nil

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
