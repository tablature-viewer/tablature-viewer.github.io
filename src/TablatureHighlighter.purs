module TablatureHighlighter where

import Prelude
import HalogenUtils

import Control.Alt (map, (<#>))
import Data.Either (Either(..))
import Data.List (List(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TablatureParser (TablatureElem(..), parseTablatureAst)
import Text.Parsing.StringParser (runParser)

highlightTablature :: forall w i. String -> List (HH.HTML w i)
highlightTablature tablatureText = case parseResult of
  Left {error, pos} -> Cons (HH.text tablatureText) Nil
  Right ast -> map renderTabElem ast
  where
  parseResult = runParser parseTablatureAst tablatureText
  renderTabElem (Number text) = HH.span [ classString "number" ] [ HH.text text ]
  renderTabElem (Other text) = HH.text text
