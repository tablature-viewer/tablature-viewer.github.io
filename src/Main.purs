module Main where

import Prelude
import QueryString
import Web.DOM.NonElementParentNode
import Web.HTML
import Web.HTML.Window

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.Element (setAttribute)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLTextAreaElement

main :: Effect Unit
main = do
  log "🍝"
  query <- getRawQueryString
  log query
  w <- window
  d <- document w
  maybeElement <- getElementById "tablature_textarea" $ toNonElementParentNode $ toDocument  d
  case maybeElement of
    Nothing -> log("error")
    Just elem -> do
      case fromElement elem of
        Nothing -> log("error")
        Just text -> do
          t <- value text
          log(t)
          setRawQueryString t
