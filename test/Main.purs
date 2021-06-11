module Test.Main where

import Prelude

import Data.Array.Partial (head)
import Data.Either (Either(..))
import Data.String (length)
import Data.String.Gen (genAsciiString, genAsciiString')
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (error)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import TablatureParser (parseCommentLine, parseEndOfLine, parseTabLine, parseTitleLine)
import Test.Assert (assert')
import Test.QuickCheck (Result(..), quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Text.Parsing.StringParser (Parser, PosString, unParser)
import Text.Parsing.StringParser.CodePoints (eof, regex, string)
import Text.Parsing.StringParser.Combinators (many)

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
      msg = "Position: " <> show rec.pos <> ";Error: " <> show rec.error <> ";In input string: " <> show inputString
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
  assertParserSuccess parseEndOfLine ""
  assertParserSuccess parseEndOfLine "\n"
  assertParserSuccess parseEndOfLine "\r"
  assertParserSuccess parseEndOfLine "\n\r"
  assertParserFailed parseEndOfLine "\r\r"
  assertParserFailed parseEndOfLine "\n\r\r"
  assertParserFailed parseEndOfLine "a"

  assertParserFailed parseCommentLine ""
  assertParserSuccess parseCommentLine "a"
  assertParserFailed parseCommentLine "123\n456"

  assertParserSuccess (many parseCommentLine) "a"
  assertParserSuccess (many parseCommentLine) "123\n456"

  quickCheck $ (\(AsciiStringNoCtrl s) -> doParseAll (parseCommentLine) false s)
  quickCheck $ (\(AsciiString s) -> doParseAll (many parseCommentLine) false s)
  assertParserSuccess (many parseCommentLine) testTablature

  assertParserFailed parseTitleLine ""
  assertParserSuccess parseTitleLine "a"
  assertParserFailed parseTitleLine "123\n456"
  assertParserSuccess parseTitleLine "---title---"
  assertParserSuccess (many parseTitleLine) "a"
  assertParserSuccess (many parseTitleLine) "123\n456"

  assertParserFailed parseTabLine ""
  assertParserFailed parseTabLine "a"
  assertParserFailed parseTabLine "|"
  assertParserFailed parseTabLine "||"
  assertParserSuccess parseTabLine "|-|"
  assertParserSuccess parseTabLine "a|-2-|a"
  assertParserSuccess parseTabLine "|-|\n"
  assertParserFailed parseTabLine "|-|\na"
  assertParserFailed parseTabLine "|-|\n|-|"
  assertParserSuccess (many parseTabLine) "|-|\n|-|"
  assertParserFailed (many parseTabLine) "|-|\na"
  assertParserFailed (many parseTabLine) "a\n|-|"
  assertParserSuccess parseTabLine (lines testTabLines # unsafePartial head)
  assertParserSuccess (many parseTabLine) testTabLines
  assertParserFailed (many parseTabLine) testTablature
  
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
