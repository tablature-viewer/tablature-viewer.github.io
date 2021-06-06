module TablatureHighlighter where

import Control.Alt (map, (<#>))
import Data.Either (Either(..))
import Data.List (List(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TablatureParser (TablatureElem(..), parseTablatureAst)
import Text.Parsing.StringParser (runParser)

highlightTablature  :: forall w i. String -> List (HH.HTML w i)
highlightTablature s = case parseResult of
  Left {error, pos} -> Cons (HH.text s) Nil
  Right ast -> map renderTabElem ast
  where
  parseResult = runParser parseTablatureAst s
  renderTabElem (Number s) = HH.i_ [ HH.text s ]
  renderTabElem (Other s) = HH.text s
