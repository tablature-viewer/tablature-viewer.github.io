module Test.Utils where

import Prelude

import Effect (Effect)
import Test.Assert (assert')
import Test.QuickCheck (Result(..))

assertSuccess :: Result -> Effect Unit
assertSuccess Success = assert' "" true
assertSuccess (Failed msg) = assert' msg false

assertFailed :: Result -> Effect Unit
assertFailed Success = assert' "Expected failure" false
assertFailed (Failed _) = assert' "" true
