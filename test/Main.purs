module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.String (length)
import Data.String.Gen (genAsciiString, genAsciiString')
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (error)
import Effect.Unsafe (unsafePerformEffect)
import TablatureParser (parseCommentLine, parseEndOfLine)
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

assertParserFailed :: forall a. Show a => Parser a -> String -> Effect Unit
assertParserFailed parser testString = doParseAll parser false testString # assertFailed

runParser :: forall a.  Parser a -> String -> Either { error :: String , pos :: Int } a
runParser p s = map (\parserResult -> parserResult.result) (unParser p { str: s, pos: 0 })

main :: Effect Unit
main = do
  log "🍝"
  assertParserSuccess parseEndOfLine ""
  assertParserSuccess parseEndOfLine "\n"
  assertParserFailed parseEndOfLine "a"

  assertParserFailed parseCommentLine ""
  assertParserSuccess parseCommentLine "a"
  assertParserFailed parseCommentLine "123\n456"

  assertParserSuccess (many parseCommentLine) "a"
  assertParserSuccess (many parseCommentLine) "123\n456"

  quickCheck $ (\(AsciiStringNoCtrl s) -> doParseAll (parseCommentLine) false s)
  quickCheck $ (\(AsciiString s) -> doParseAll (many parseCommentLine) false s)
  assertParserSuccess (many parseCommentLine) testTablature
  
  
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
