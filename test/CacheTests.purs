module CacheTests where

import Prelude

import Effect (Effect)
import Test.Assert (assert')
import Test.QuickCheck (Result(..), quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary)

main :: Effect Unit
main = do
  pure unit
