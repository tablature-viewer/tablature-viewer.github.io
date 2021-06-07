module TablatureHighlighter where

import HalogenUtils
import Prelude
import TablatureParser
import Utils

import Control.Alt (map, (<#>))
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.List (List(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Text.Parsing.StringParser (runParser)

highlightTablature :: forall w i. String -> List (HH.HTML w i)
highlightTablature tablatureText = case parseResult of
  Left {error, pos} -> unsafeError ("at " <> show pos <> ": " <> error) # \_ -> Cons (HH.text tablatureText) Nil
  Right ast -> renderTabAst ast
  where
  parseResult = runParser parseTabAst tablatureText

renderTabAst :: forall w i. TabAst -> List (HH.HTML w i)
renderTabAst ast = map renderLine ast
  where
  renderLine :: Line -> HH.HTML w i
  renderLine (MiscLine line) = HH.text line
  renderLine (TitleLine line) = HH.text line.title
  renderLine (TabLine line) = HH.span_ $ fromFoldable (map renderTabLineElem line)
  renderTabLineElem (Prefix string) = HH.text string
  renderTabLineElem (Suffix string) = HH.text string
  renderTabLineElem (TimeLine string) = HH.span [ classString "number" ] [ HH.text string ]
  renderTabLineElem (Fret string) = HH.text string
  renderTabLineElem (Special string) = HH.text string
