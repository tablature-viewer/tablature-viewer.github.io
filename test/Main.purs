module Test.Main where

import Prelude

import Effect (Effect)
import ParserTests as ParserTests

main :: Effect Unit
main = do
  ParserTests.main
