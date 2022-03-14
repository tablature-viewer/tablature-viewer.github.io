module ParserTests where

import Prelude

import Control.Alt ((<|>))
import Data.Array.Partial (head)
import Data.Either (Either(..))
import Data.String (drop)
import Data.String.Gen (genAsciiString, genAsciiString', genUnicodeString)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (error)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import StringParser (Parser, eof, lookAhead, many, manyTill, regex, string, unParser)
import TablatureParser (parseAnyLine, parseChordLegend, parseEndOfLine, parseInnerTablatureLine, parseTablatureDocument, parseTablatureLine, parseTextLine, parseTitleLine)
import Test.QuickCheck (Result(..), quickCheck, quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.Utils (assertFailed, assertSuccess)

-- TODO: improve tests

newtype AsciiString = AsciiString String
newtype UnicodeString = UnicodeString String
newtype AsciiStringNoCtrl = AsciiStringNoCtrl String

instance Arbitrary UnicodeString where
  arbitrary = genUnicodeString <#> UnicodeString

instance Arbitrary AsciiString where
  arbitrary = genAsciiString' <#> AsciiString

instance Arbitrary AsciiStringNoCtrl where
  arbitrary = genAsciiString <#> AsciiStringNoCtrl

main :: Effect Unit
main = do
  log "🍝"

  -- This used to hang
  assertParserFailed (parseChordLegend) "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

  -- many should make sure that many doesn't hang
  assertParserSuccess (many eof) ""
  assertParserSuccess (many ((string "1" *> pure unit) <|> eof)) "1"

  -- many should succeed when parser doesn't consume and stop looping
  assertParserSuccess ((many (regex "2?")) *> string "1") "1"
  -- manyTill should fail when parser doesn't consume
  assertParserFailed ((manyTill (regex "2?") (string "2")) *> string "1") "1"

  -- Check if manyTill works the way we expect it to
  assertParserFailed (manyTill (string "1") (string "2")) "111111"
  assertParserSuccess (manyTill (string "1") (string "2")) "1111112"
  assertParserFailed (manyTill (string "1") (lookAhead $ string "2")) "1111112"
  assertParserSuccess ((manyTill (string "1") (lookAhead $ string "2")) *> (string "2")) "1111112"
  assertParserSuccess ((manyTill (regex ".") (lookAhead $ string "2")) *> (string "2")) "1111112"
  assertParserSuccess ((manyTill (regex ".") (lookAhead $ string "2")) *> (string "2")) "2"

  assertParserSuccess parseEndOfLine ""
  assertParserSuccess parseEndOfLine "\n"

  assertParserSuccess parseTextLine "a"
  assertParserSuccess (many parseTextLine) "a"
  assertParserSuccess (many parseTextLine) "123\n456"
  assertParserSuccess (many parseTextLine) testTablature

  assertParserFailed parseTitleLine ""
  assertParserSuccess parseTitleLine "a"
  assertParserFailed parseTitleLine "123\n456"
  assertParserSuccess parseTitleLine "---title---"
  assertParserSuccess (many parseTitleLine) "a"
  assertParserSuccess (many parseTitleLine) "123\n456"

  assertParserSuccess (parseInnerTablatureLine) "|-- --|"
  assertParserSuccess (parseTablatureLine) "B|----------------------||o-------------------------|-----------7h8p7-----------|"
  assertParserSuccess (parseTablatureLine) "||o-4p0h7p0h4p0h7p0h4p0h7p0h4p0h7p0h|=4p0h7p0h4p0h7p0h4p0h7p0h4p0h7p0--|"
  assertParserSuccess (parseTablatureLine) "|---|\n"
  assertParserSuccess (parseTablatureLine) (lines testTabLines # unsafePartial head)
  assertParserFailed (parseTablatureLine) ""
  assertParserFailed (parseTablatureLine) "a"
  assertParserFailed (parseTablatureLine) "|---|\na"
  assertParserFailed (parseTablatureLine) "|---|\n|---|"
  assertParserSuccess (many (parseTablatureLine)) "|---|\n|---|"
  assertParserSuccess (many (parseTablatureLine)) testTabLines
  assertParserFailed (many (parseTablatureLine)) "|---|\na"
  assertParserFailed (many (parseTablatureLine)) "a\n|---|"
  assertParserFailed (many (parseTablatureLine)) testTablature

  assertParserSuccess (parseTablatureDocument) ""
  assertParserSuccess (parseTablatureDocument) "asdf"
  assertParserSuccess (parseTablatureDocument) "   asdf   "
  assertParserSuccess (parseTablatureDocument) "||"
  assertParserSuccess (parseTablatureDocument) "|---|"
  assertParserSuccess (parseTablatureDocument) "|---|\na"
  assertParserSuccess (parseTablatureDocument) "|---|\n|---|"
  assertParserSuccess (parseTablatureDocument) "|---|\r\n|---|"
  assertParserSuccess (parseTablatureDocument) "|---|\r\n\r|---|"
  assertParserSuccess (parseTablatureDocument) testTabLines
  assertParserSuccess (parseTablatureDocument) testTablature

  quickCheck' 10000 \(AsciiStringNoCtrl s) -> doParseAll (parseTextLine) false s
  quickCheck' 10000 \(AsciiString s) -> doParseAll (many parseAnyLine) false s
  quickCheck' 10000 \(UnicodeString s) -> doParseAll (many parseAnyLine) false s
  quickCheck' 10000 \(AsciiStringNoCtrl s) -> doParseAll (parseTablatureDocument) false s
  quickCheck' 10000 \(AsciiString s) -> doParseAll (parseTablatureDocument) false s
  quickCheck' 10000 \(UnicodeString s) -> doParseAll (parseTablatureDocument) false s
  quickCheck' 10000 $ doParseAll parseTablatureDocument false

testTabLines :: String
testTabLines =
  """e|---------------------------------------------------------------------------|
B|---------------------------------------------------------------------------|
G|------7-----7h9p7----------------7--7--------9-----------------------------| Some comment
D|------7-----7-----9p7-9p7--------7--7--------9-----------------------------|
A|------9-------------------9---/5-------5/7-7-------------------------------|
E|--/7------7----------------------------------------------------------------|"""

testTablature :: String
testTablature =
  """-------------------------------------------------------------------------------
                             Aftermath - Muse
-------------------------------------------------------------------------------
Tabbed by: Muser00
Tuning: Standard
 
[Intro] 0:39

e|---------------------------------------------------------------------------|
B|---------------------------------------------------------------------------|
G|------7-----7h9p7----------------7--7--------9-----------------------------|
D|------7-----7-----9p7-9p7--------7--7--------9-----------------------------|
A|------9-------------------9---/5-------5/7-7-------------------------------|
E|--/7------7----------------------------------------------------------------|"""

doParseAll :: forall a. Show a => Parser a -> Boolean -> String -> Result
doParseAll parser verbose inputString = unsafePerformEffect do
  if verbose then log $ "Parsing '" <> inputString <> "'" else pure unit
  result <- case unParser (parser <* eof) { substring: inputString, position: 0 } of
    Left rec -> do
      if verbose then error $ msg else pure unit
      pure $ Failed msg
      where
      msg = "Position: " <> show rec.pos
        <> "\nError: "
        <> show rec.error
        <> "\nIn input string: "
        <> inputString
        <> "\nWith unparsed suffix: "
        <> (show $ drop rec.pos inputString)
    Right rec -> do
      if verbose then log $ "Result was: " <> show rec.result <> "; Suffix was: " <> show rec.suffix else pure unit
      pure Success
  if verbose then log "-----" else pure unit
  pure result

assertParserSuccess :: forall a. Show a => Parser a -> String -> Effect Unit
assertParserSuccess parser testString = doParseAll parser false testString # assertSuccess

assertParserSuccess' :: forall a. Show a => Parser a -> String -> Effect Unit
assertParserSuccess' parser testString = doParseAll parser true testString # assertSuccess

assertParserFailed :: forall a. Show a => Parser a -> String -> Effect Unit
assertParserFailed parser testString = doParseAll parser false testString # assertFailed

assertParserFailed' :: forall a. Show a => Parser a -> String -> Effect Unit
assertParserFailed' parser testString = doParseAll parser true testString # assertFailed

runParser :: forall a. Parser a -> String -> Either { error :: String, pos :: Int } a
runParser p s = map (\parserResult -> parserResult.result) (unParser p { substring: s, position: 0 })
