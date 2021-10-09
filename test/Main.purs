module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array.Partial (head)
import Data.Either (Either(..))
import Data.String (drop, length)
import Data.String.Gen (genAsciiString, genAsciiString')
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (error)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import TablatureParser (assertConsume, parseEndOfLine, parseTablatureDocument, parseTablatureLine, parseTextLine, parseTitleLine)
import Test.Assert (assert')
import Test.QuickCheck (Result(..), quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Text.Parsing.StringParser (Parser, PosString, unParser)
import Text.Parsing.StringParser.CodePoints (eof, regex, string)
import Text.Parsing.StringParser.Combinators (lookAhead, many, manyTill)

newtype AsciiString = AsciiString String
newtype AsciiStringNoCtrl = AsciiStringNoCtrl String

instance arbitraryAsciiString :: Arbitrary AsciiString where
  arbitrary = genAsciiString' <#> AsciiString

instance arbitraryAsciiStringNoCtrl :: Arbitrary AsciiStringNoCtrl where
  arbitrary = genAsciiString <#> AsciiStringNoCtrl

endOfInputString :: PosString -> Boolean
endOfInputString suffix = length suffix.str == suffix.pos

doParseAll :: forall a. Show a => Parser a -> Boolean -> String -> Result
doParseAll parser verbose inputString = unsafePerformEffect do
  if verbose then log $ "Parsing '" <> inputString <> "'" else pure unit
  result <- case unParser (parser <* eof) { str: inputString, pos: 0 } of
    Left rec -> do
      if verbose then error $ msg else pure unit
      pure $ Failed msg
      where
      msg = "Position: " <> show rec.pos
        <> "\nError: " <> show rec.error
        <> "\nIn input string: " <> inputString
        <> "\nWith unparsed suffix: " <> (show $ drop rec.pos inputString)
    Right rec -> do
      if verbose then log $ "Result was: " <> show rec.result <> "; Suffix was: " <> show rec.suffix else pure unit
      pure Success
  if verbose then log "-----" else pure unit
  pure result

assertSucess :: Result -> Effect Unit
assertSucess Success = assert' "" true
assertSucess (Failed msg) = assert' msg false

assertFailed :: Result -> Effect Unit
assertFailed Success = assert' "Expected failure" false
assertFailed (Failed _) = assert' "" true

assertParserSuccess :: forall a. Show a => Parser a -> String -> Effect Unit
assertParserSuccess parser testString = doParseAll parser false testString # assertSucess

assertParserSuccess' :: forall a. Show a => Parser a -> String -> Effect Unit
assertParserSuccess' parser testString = doParseAll parser true testString # assertSucess

assertParserFailed :: forall a. Show a => Parser a -> String -> Effect Unit
assertParserFailed parser testString = doParseAll parser false testString # assertFailed

assertParserFailed' :: forall a. Show a => Parser a -> String -> Effect Unit
assertParserFailed' parser testString = doParseAll parser true testString # assertFailed

runParser :: forall a.  Parser a -> String -> Either { error :: String , pos :: Int } a
runParser p s = map (\parserResult -> parserResult.result) (unParser p { str: s, pos: 0 })

main :: Effect Unit
main = do
  log "🍝"

  -- assertConsume should make sure that many doesn't hang
  assertParserSuccess (many (assertConsume eof)) ""
  assertParserSuccess (many (assertConsume ((string "1" *> pure unit) <|> eof))) "1"

  -- TODO: be able to parse lines not ending in newlines

  -- Check if manyTill works the way we expect it to
  assertParserFailed (manyTill (string "1") (string "2") ) "111111"
  assertParserSuccess (manyTill (string "1") (string "2") ) "1111112"
  assertParserFailed (manyTill (string "1") (lookAhead $ string "2") ) "1111112"
  assertParserSuccess ( (manyTill (string "1") (lookAhead $ string "2")) *> (string "2") ) "1111112"
  assertParserSuccess ( (manyTill (regex ".") (lookAhead $ string "2")) *> (string "2") ) "1111112"
  assertParserSuccess ( (manyTill (regex ".") (lookAhead $ string "2")) *> (string "2") ) "2"

  -- assertParserSuccess parseEndOfLine ""
  -- assertParserFailed parseEndOfLine "a"
  assertParserSuccess parseEndOfLine "\n"
  assertParserSuccess parseEndOfLine "\r"
  assertParserSuccess parseEndOfLine "\n\r"
  assertParserFailed parseEndOfLine "\r\r"
  assertParserFailed parseEndOfLine "\n\r\r"

  assertParserFailed parseTextLine ""
  -- assertParserSuccess parseTextLine "a"
  assertParserFailed parseTextLine "123\n456"

  -- assertParserSuccess (many parseTextLine) "a"
  -- assertParserSuccess (many parseTextLine) "123\n456"

  -- quickCheck $ (\(AsciiStringNoCtrl s) -> doParseAll (parseTextLine) false s)
  -- quickCheck $ (\(AsciiString s) -> doParseAll (many parseTextLine) false s)
  -- assertParserSuccess (many parseTextLine) testTablature

  -- assertParserFailed parseTitleLine ""
  -- assertParserSuccess parseTitleLine "a"
  -- assertParserFailed parseTitleLine "123\n456"
  -- assertParserSuccess parseTitleLine "---title---"
  -- assertParserSuccess (many parseTitleLine) "a"
  -- assertParserSuccess (many parseTitleLine) "123\n456"

  -- assertParserFailed (parseTablatureLine) ""
  -- assertParserFailed (parseTablatureLine) "a"
  -- assertParserSuccess (parseTablatureLine) "B|----------------------||o-------------------------|-----------7h8p7-----------|"
  -- assertParserSuccess (parseTablatureLine) "||o-4p0h7p0h4p0h7p0h4p0h7p0h4p0h7p0h|=4p0h7p0h4p0h7p0h4p0h7p0h4p0h7p0--|"
  -- assertParserSuccess (parseTablatureLine) "|---|\n"
  -- assertParserFailed (parseTablatureLine) "|---|\na"
  -- assertParserFailed (parseTablatureLine) "|---|\n|---|"
  -- assertParserSuccess (many (parseTablatureLine)) "|---|\n|---|"
  -- assertParserFailed (many (parseTablatureLine)) "|---|\na"
  -- assertParserFailed (many (parseTablatureLine)) "a\n|---|"
  -- assertParserSuccess (parseTablatureLine) (lines testTabLines # unsafePartial head)
  -- assertParserSuccess (many (parseTablatureLine)) testTabLines
  -- assertParserFailed (many (parseTablatureLine)) testTablature

  -- assertParserSuccess (parseTablatureDocument) ""
  -- assertParserSuccess (parseTablatureDocument) "asdf"
  -- assertParserSuccess (parseTablatureDocument) "   asdf   "
  -- assertParserSuccess (parseTablatureDocument) "||"
  -- assertParserSuccess (parseTablatureDocument) "|---|"
  -- assertParserSuccess (parseTablatureDocument) "|---|\na"
  -- assertParserSuccess (parseTablatureDocument) "|---|\n|---|"
  -- assertParserSuccess (parseTablatureDocument) testTabLines
  -- assertParserSuccess (parseTablatureDocument) testTablature
  -- quickCheck $ (\(AsciiStringNoCtrl s) -> doParseAll (parseTablatureDocument ) false s)
  -- quickCheck $ (\(AsciiString s) -> doParseAll (parseTablatureDocument ) false s)
  -- quickCheck $ doParseAll (parseTablatureDocument ) false

testTabLines :: String
testTabLines = """e|---------------------------------------------------------------------------|
B|---------------------------------------------------------------------------|
G|------7-----7h9p7----------------7--7--------9-----------------------------| Some comment
D|------7-----7-----9p7-9p7--------7--7--------9-----------------------------|
A|------9-------------------9---/5-------5/7-7-------------------------------|
E|--/7------7----------------------------------------------------------------|"""
  
testTablature :: String
testTablature = """
-------------------------------------------------------------------------------
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
