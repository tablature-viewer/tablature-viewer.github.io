module TablatureParser where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (fold, sum)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.String (drop, length, toLower, toUpper)
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Utils (startsWith)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import StringParser (Parser, eof, fail, many, manyTill, option, optionMaybe, regex, string, try, tryAhead, unParser)
import TablatureDocument (Chord(..), ChordLegendElem(..), ChordLineElem(..), ChordMod(..), HeaderLineElem(..), Note(..), NoteLetter(..), Spaced(..), TablatureDocument, TablatureDocumentLine(..), TablatureLineElem(..), TextLineElem(..), TitleLineElem(..), fromString)
import Utils (print)

-- TODO: Improve the parser code

parseTablatureDocument :: Parser TablatureDocument
parseTablatureDocument = do
  commentLinesBeforeTitle <- option Nil $ (try $ manyTill parseTextLine (tryAhead (parseTitleLine <|> parseTablatureLinesWithSurroundings)))
  title <- option Nil $ parseTitleLine <#> \result -> result : Nil
  body <- many (parseTablatureLinesWithSurroundings <|> parseChordLine <|> parseHeaderLine <|> parseTextLine <|> parseAnyLine)
  pure $ commentLinesBeforeTitle <> title <> body

parseTitleLine :: Parser TablatureDocumentLine
parseTitleLine = try do
  prefix <- regex """[^\w\n]*"""
  title <- regex """[^\n]*[\w)!?"']"""
  suffix <- regex """[^\n]*""" <* parseEndOfLine
  pure $ TitleLine $ TitleOther prefix : Title title : TitleOther suffix : Nil

parseTimelineSep :: Parser (List TablatureLineElem)
parseTimelineSep = regex """[|\[\]]{1,2}""" <#> TimelineSep <#> List.singleton

parseTimelineConnection :: Parser (List TablatureLineElem)
parseTimelineConnection = regex """[\-—]+""" <#> TimelineConnection <#> List.singleton

-- Something is a timeline space if there is still another timeline sep or connection down the line somwhere
-- In other words, a timeline space cannot be at the end of a timeline
parseTimelineSpace :: Parser (List TablatureLineElem)
parseTimelineSpace = try (regex """[ ]{1,2}""" <* tryAhead (regex """[^\s]""") <* assertTimelineNotEnded) <#> TimelineSpace <#> List.singleton

assertTimelineNotEnded :: Parser Unit
assertTimelineNotEnded = pure unit <* tryAhead (manyTill parseAnyChar (parseTimelineConnection <|> parseTimelineSep))

parseFret :: Parser (List TablatureLineElem)
parseFret = regex ("""[\d↊↋]+""") <#> Fret <#> List.singleton

parseSpecial :: Parser (List TablatureLineElem)
parseSpecial = regex ("""[^\s|\[\]\-—\d↊↋]+""") <#> Special <#> List.singleton

parseMaybeTuning :: Parser (Maybe TablatureLineElem)
parseMaybeTuning = optionMaybe $ parseSpacedNote <* (tryAhead parseTimelineSep) <#> Tuning

parseTablatureLinesWithSurroundings :: Parser TablatureDocumentLine
parseTablatureLinesWithSurroundings = try do
  tabLine <- parseTablatureLinesWithPrefix
  suffix <- regex """[^\n]*""" <* parseEndOfLine <#> Suffix
  pure $ TablatureLine $ tabLine <> suffix : Nil

-- We can parse multiple subsequence tablines on the same line
-- This is because sometimes systems are positioned side by side
parseTablatureLinesWithPrefix :: Parser (List TablatureLineElem)
parseTablatureLinesWithPrefix = try do
  prefix <- manyTill (regex """[^\n]""") (tryAhead parseTablatureLineWithOptionalTuning) <#> \result -> Prefix (fold result)
  tabLine <- parseTablatureLineWithOptionalTuning
  subsequentTabLine <- option Nil parseTablatureLinesWithPrefix
  pure $ prefix : Nil <> tabLine <> subsequentTabLine

parseTablatureLineWithOptionalTuning :: Parser (List TablatureLineElem)
parseTablatureLineWithOptionalTuning = try do
  maybeTuning <- parseMaybeTuning
  tabLine <- parseInnerTablatureLine
  startsWithBracket <- pure
    case List.head tabLine of
      Just (TimelineSep x) -> if startsWith "[" x then true else false
      _ -> false
  if startsWithBracket && isNothing maybeTuning then fail "If the tablature starts with [ then we require a tuning to make sure we don't label a header as a tab line"
  else pure $ getTuning maybeTuning <> tabLine
  where
  getTuning = case _ of
    Nothing -> Nil
    Just t -> t : Nil

-- A timeline can end with a fret or special if there were no spaces or sep right before
-- Since we cannot do a lookbehind we do it like this
parseParseTimelineConnectionWithSubsequentFretsAndSpecials :: Parser (List TablatureLineElem)
parseParseTimelineConnectionWithSubsequentFretsAndSpecials = try do
  timeline <- parseTimelineConnection
  fretsAndSpecials <- many (parseFret <|> parseSpecial)
  pure $ timeline <> List.concat fretsAndSpecials

parseInnerTablatureLine :: Parser (List TablatureLineElem)
parseInnerTablatureLine = try do
  result <-
    tryAhead parseTimelineSep *> many
      (parseParseTimelineConnectionWithSubsequentFretsAndSpecials <|> parseTimelineSep <|> parseTimelineSpace <|> try (parseFret <* assertTimelineNotEnded) <|> try (parseSpecial <* assertTimelineNotEnded))
      <#> List.concat
  -- Do a post check to see if there are enough dashes
  let nrSpaces = result <#> countTimeLineSpaces # sum
  let nrTimelineChars = result <#> countTimeLineChars # sum
  let nrTablineChars = result <#> countChars # sum
  if nrSpaces < nrTimelineChars && 10 < nrTablineChars then pure result else fail "Not enough tablature line characters"
  where
  countChars s = String.length (print s)
  countTimeLineSpaces (TimelineSpace s) = String.length s
  countTimeLineSpaces _ = 0
  countTimeLineChars (TimelineConnection s) = String.length s
  countTimeLineChars (TimelineSep s) = String.length s
  countTimeLineChars _ = 0

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
  parseStackableChordMods = regex """(o|no|add|dim|dom|augm(?![a-zA-Z])|aug|maj|Maj|M|ø|Δ|[*])?([2-9]|10|11|12|13)?(b|#|[+]|[-])?"""
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
  tryAhead (regex """(?<!\S)(([\dxX↊↋]{1,2}[-]*){4,12})(?!\S)""")
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
  # Regex.replace (unsafeRegex "\r" global) "" --universal line endings
  # Regex.replace (unsafeRegex "[ \t]+\n" global) "\n" -- trim lines

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
