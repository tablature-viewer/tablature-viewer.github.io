module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Assert (assert)

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."
  assert (1 == 1)
