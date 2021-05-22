module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import QueryString

main :: Effect Unit
main = do
  log "🍝"
  query <- getQueryString
  log query
