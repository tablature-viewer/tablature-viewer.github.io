module Test.Main where

import Prelude

import Effect (Effect)
import ParserTests as ParserTests
import CacheTests as CacheTests

main :: Effect Unit
main = do
  CacheTests.main
  ParserTests.main
