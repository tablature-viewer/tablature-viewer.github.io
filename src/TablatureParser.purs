module TablatureParser where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Foldable (sum)
import Data.List (List)
import Text.Parsing.StringParser (Parser, fail, runParser, unParser)
import Text.Parsing.StringParser.CodePoints (regex, string)
import Text.Parsing.StringParser.Combinators (choice, many)

data TablatureElem = Number String | Other String
type TablatureAst = List TablatureElem

exampleContent1 :: String
exampleContent1 = "asdf 1234 45 fds23"

parseNumber :: Parser TablatureElem
parseNumber = regex "\\d+" <#> \result -> Number result

parseOther :: Parser TablatureElem
parseOther = regex "\\D+" <#> \result -> Other result

parseTablatureElem :: Parser TablatureElem
parseTablatureElem = choice
  [ parseNumber
  , parseOther ]

parseTablatureAst :: Parser TablatureAst
parseTablatureAst = many parseTablatureElem

