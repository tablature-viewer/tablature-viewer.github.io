module TablatureHighlighter where

import HalogenUtils
import Prelude
import TablatureParser
import Utils

import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

highlightTablature :: forall w i. String -> List (HH.HTML w i)
highlightTablature tablatureText = case parseResult of
  Nothing -> HH.text tablatureText : Nil
  Just tablatureDoc -> renderTablatureDocument tablatureDoc
  where
  parseResult = tryRunParser parseTablatureDocument tablatureText

renderTablatureDocument :: forall w i. TablatureDocument -> List (HH.HTML w i)
renderTablatureDocument ast = map renderLine ast
  where
  renderLine :: TablatureDocumentLine -> HH.HTML w i
  renderLine (CommentLine line) = HH.text $ line <> "\n"
  renderLine (TitleLine line) = HH.text $ line.prefix <> line.title <> line.suffix <> "\n"
  renderLine (TablatureLine line) = HH.span_ $ fromFoldable $ map renderTablatureElem line <> HH.text "\n" : Nil
  renderTablatureElem (Prefix string) = HH.text string
  renderTablatureElem (Suffix string) = HH.text string
  renderTablatureElem (TimeLine string) = HH.span [ classString "number" ] [ HH.text string ]
  renderTablatureElem (Fret string) = HH.text string
  renderTablatureElem (Special string) = HH.text string
