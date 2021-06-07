module TablatureParser where

import Prelude hiding (between)

import Data.List (List(..),(:))
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (regex)
import Text.Parsing.StringParser.Combinators (choice, many)

type TabAst = List Line

data Line
  = TitleLine {prefix::String, title::String, suffix::String}
  | TabLine (List TabLineElem)
  | MiscLine String

data TabLineElem
  = Prefix String
  | Suffix String
  | TimeLine String
  | Fret String
  | Special String

parseTabAst :: Parser TabAst
parseTabAst = many parseLine

parseLine :: Parser Line
parseLine = choice
  [ parseTitleLine
  -- , parseTabLine
  -- , parseMiscLine
  ]

parseTitleLine :: Parser Line
parseTitleLine = regex ".*" <#> \result -> TitleLine {prefix:"", title:result, suffix:""}

parseTabLine :: Parser Line
parseTabLine = do
  p <- parsePrefix
  t <- parseTimeLine
  s <- parseSuffix
  pure $ TabLine (p:t:s:Nil)
  where
  parsePrefix = regex "^.*?(?=|)" <#> \result -> Prefix result
  parseTimeLine = regex "|.*|" <#> \result -> TimeLine result
  parseSuffix = regex "(?<=|).*$)" <#> \result -> Suffix result

parseMiscLine :: Parser Line
parseMiscLine = regex ".*" <#> \result -> MiscLine result

