module TablatureParser where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (fold, foldr, sum)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromJust)
import Data.String (drop, length, toLower, toUpper)
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import StringParser (Parser, eof, fail, many, manyTill, option, optionMaybe, regex, string, try, tryAhead, unParser)
import TablatureDocument (Chord(..), ChordLegendElem(..), ChordLineElem(..), ChordMod(..), HeaderLineElem(..), Note(..), NoteLetter(..), Spaced(..), TablatureDocument, TablatureDocumentLine(..), TablatureLineElem(..), TextLineElem(..), TitleLineElem(..), fromString)

-- TODO: Improve the parser code

parseTablatureDocument :: Parser TablatureDocument
parseTablatureDocument = do
  commentLinesBeforeTitle <- option Nil $ (try $ manyTill parseTextLine (tryAhead (parseTitleLine <|> parseTablatureLineWithSurroundings)))
  title <- option Nil $ parseTitleLine <#> \result -> result : Nil
  body <- many (parseTablatureLineWithSurroundings <|> parseChordLine <|> parseHeaderLine <|> parseTextLine <|> parseAnyLine)
  pure $ commentLinesBeforeTitle <> title <> body

parseTitleLine :: Parser TablatureDocumentLine
parseTitleLine = try do
  prefix <- regex """[^\w\n]*"""
  title <- regex """[^\n]*[\w)!?"']"""
  suffix <- regex """[^\n]*""" <* parseEndOfLine
  pure $ TitleLine $ TitleOther prefix : Title title : TitleOther suffix : Nil

parseTimelineSep :: Parser TablatureLineElem
parseTimelineSep = regex """[|\[\]]{1,2}""" <#> TimelineSep

parseTimelineConnection :: Parser TablatureLineElem
parseTimelineConnection = regex """[\-—]+""" <#> TimelineConnection

-- Something is a timeline space if there is still another timeline sep or connection down the line somwhere
-- In other words, a timeline space cannot be at the end of a timeline
parseTimelineSpace :: Parser TablatureLineElem
parseTimelineSpace = try (regex """[ ]+""" <* assertTimelineNotEnded) <#> TimelineSpace

assertTimelineNotEnded :: Parser Unit
assertTimelineNotEnded = pure unit <* tryAhead (manyTill parseAnyChar (parseTimelineConnection <|> parseTimelineSep))

parseFret :: Parser TablatureLineElem
parseFret = try (regex ("""[\d↊↋]+""") <* assertTimelineNotEnded) <#> Fret

parseSpecial :: Parser TablatureLineElem
parseSpecial = try (regex ("""[^\s|\[\]\-—\d↊↋]+""") <* assertTimelineNotEnded <#> Special)

parseMaybeTuning :: Parser (Maybe TablatureLineElem)
parseMaybeTuning = optionMaybe $ parseSpacedNote <* (tryAhead parseTimelineSep) <#> Tuning

parseTablatureLineWithSurroundings :: Parser TablatureDocumentLine
parseTablatureLineWithSurroundings = try do
  prefix <- manyTill (regex """[^\n]""") (tryAhead parseTablatureLineWithOptionalTuning) <#> \result -> Prefix (fold result)
  tabLine <- parseTablatureLineWithOptionalTuning
  suffix <- regex """[^\n]*""" <* parseEndOfLine <#> Suffix
  pure $ TablatureLine $ prefix : Nil <> tabLine <> suffix : Nil

parseTablatureLineWithOptionalTuning :: Parser (List TablatureLineElem)
parseTablatureLineWithOptionalTuning = try do
  maybeTuning <- parseMaybeTuning
  tabLine <- parseInnerTablatureLine
  pure $ getTuning maybeTuning <> tabLine
  where
  getTuning = case _ of
    Nothing -> Nil
    Just t -> t : Nil

parseInnerTablatureLine :: Parser (List TablatureLineElem)
parseInnerTablatureLine = try do
  result <- tryAhead parseTimelineSep *> many
    (parseTimelineConnection <|> parseTimelineSep <|> parseTimelineSpace <|> parseFret <|> parseSpecial)
  -- Do a post check to see if there are enough dashes
  if (result <#> countTimeLineElems # sum) > 4 then pure result else fail "Not enough dashes to qualify as tab line"
  where
  countTimeLineElems (TimelineConnection s) = String.length s
  countTimeLineElems (TimelineSep s) = String.length s
  countTimeLineElems _ = 0

parseHeaderLine :: Parser TablatureDocumentLine
parseHeaderLine = try do
  header <- regex """[^\S\n]*\[[^\n]*\w{2}[^\n]*\]"""
  suffix <- regex """[^\n]*""" <* parseEndOfLine
  pure $ HeaderLine ((Header header) : (HeaderSuffix suffix) : Nil)

parseChordLine :: Parser TablatureDocumentLine
parseChordLine = try do
  (many parseChordComment <> (parseChordLineChord <#> \c -> c : Nil) <> many (parseChordLineChord <|> parseChordComment) <* parseEndOfLine) <#> ChordLine

parseChordLineChord :: Parser ChordLineElem
parseChordLineChord = (parseChord <#> \chord -> ChordLineChord chord)

parseChord :: Parser (Spaced Chord)
parseChord = do
  root <- parseNote
  chordType <- parseChordType
  mods <- parseChordMods <#> fold
  maybeBass <- optionMaybe (string "/" *> parseNote)
  assertEndChordBoundary
  spaceSuffix <- parseSpaces
  pure $ Spaced
    { elem: Chord
        { root: root
        , type: chordType
        , mods: (ChordMod { pre: "", interval: mods, post: "" }) : Nil
        , bass: maybeBass
        }
    , spaceSuffix: length spaceSuffix
    }
  where
  parseChordType = regex """(ø|Δ| ?Major| ?major|Maj|maj|Ma| ?Minor| ?minor|Min|min|M|m|[-]|[+]|o)?"""
  parseStackableChordMods = regex """(o|no|add|dim|dom|augm(?![a-zA-Z])|aug|maj|Maj|M|ø|Δ)?([2-9]|10|11|12|13)?(b|#|[+]|[-])?"""
  parseParenthesizedStackableChordMods = try (string "(" <> parseStackableChordMods <> string ")")
  parseChordMods = many (regex """sus[24]?""" <|> parseParenthesizedStackableChordMods <|> parseStackableChordMods)

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
parseChordComment = regex """[^\S\n]*(\([^\n()]*\)|\.\.+|[^\S\n]+|x\d+)[^\S\n]*""" <#> ChordComment

parseTextLine :: Parser TablatureDocumentLine
parseTextLine = try do
  manyTill (parseTextLineSpace <|> (parsePunctuation <#> Text) <|> try (parseChord <#> TextLineChord) <|> parseChordLegend <|> parseWord) parseEndOfLine
    <#> TextLine

parseTextLineSpace :: Parser TextLineElem
parseTextLineSpace = do
  regex """[^\S\n]+""" <#> Spaces

-- Parse stuff that will not be part of a chord but can still be right next to a chord
parsePunctuation :: Parser String
parsePunctuation = regex """[,.?())]"""

assertEndChordBoundary :: Parser Unit
assertEndChordBoundary = eof <|> tryAhead (regex """\s""" <|> parsePunctuation) *> pure unit

parseSpaces :: Parser String
parseSpaces = regex """[ ]*"""

parseWord :: Parser TextLineElem
parseWord = regex """(?<!\S)\S+(?!\S)""" <#> Text

parseChordLegend :: Parser TextLineElem
parseChordLegend = do
  -- Watch out for catastrophic backtracking.
  tryAhead (regex """(?<!\S)(([\dxX↊↋]{1,2}[-]*){3,12})(?!\S)""")
    *> many ((regex """[1234567890↊↋]""" <#> ChordFret) <|> (regex """[xX-]""" <#> ChordSpecial)) <#> ChordLegend

-- This is a backup in case the other parsers fail
parseAnyLine :: Parser TablatureDocumentLine
parseAnyLine = do
  regex """[^\n]*""" <* parseEndOfLine <#> \result -> TextLine ((Text result) : Nil)

parseEndOfLine :: Parser Unit
parseEndOfLine = parseEndOfLineString *> pure unit <|> eof

parseEndOfLineString :: Parser String
parseEndOfLineString = regex """\n"""

parseAnyChar :: Parser String
parseAnyChar = regex """[^\n]"""

sanitizeInput :: String -> String
sanitizeInput inputString = inputString
  # Regex.replace (unsafeRegex "\r" global) ""
  # Regex.replace (unsafeRegex "[ \t]+\n" global) "\n"

tryParseTablature :: String -> Maybe TablatureDocument
tryParseTablature inputString = tryRunParser parseTablatureDocument (sanitizeInput inputString)

tryRunParser :: forall a. Show a => Parser a -> String -> Maybe a
tryRunParser parser inputString =
  case unParser (parser <* eof) { substring: inputString, position: 0 } of
    Left rec -> Console.error msg # unsafePerformEffect # \_ -> Nothing
      where
      msg = "Position: " <> show rec.pos
        <> "\nError: "
        <> show rec.error
        <> "\nIn input string: "
        <> inputString
        <> "\nWith unparsed suffix: "
        <> (show $ drop rec.pos inputString)
    Right rec -> pure rec.result
