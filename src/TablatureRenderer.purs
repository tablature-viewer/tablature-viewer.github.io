module TablatureRenderer where

import HalogenUtils
import Prelude
import TablatureParser
import Utils

import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Int (Radix, decimal, radix, toStringAs)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), replace, replaceAll)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

renderTablature :: forall w i. String -> List (HH.HTML w i)
renderTablature tablatureText = case parseResult of
  Nothing -> HH.text tablatureText : Nil
  Just tablatureDoc -> renderTablatureDocument tablatureDoc
  where
  parseResult = tryRunParser parseTablatureDocument tablatureText

renderTablatureDocument :: forall w i. TablatureDocument -> List (HH.HTML w i)
renderTablatureDocument doc = map renderLine doc
  where
  renderLine :: TablatureDocumentLine -> HH.HTML w i
  renderLine (CommentLine line) = HH.span [ classString "tabComment" ] [HH.text $ line <> "\n"]
  renderLine (TitleLine line) = HH.span_
   [ HH.span [ classString "tabComment" ] [HH.text $ line.prefix]
   , HH.span [ classString "tabTitle" ] [HH.text $ line.title]
   , HH.span [ classString "tabComment" ] [HH.text $ line.suffix <> "\n"]
   ]
  renderLine (TablatureLine line) = HH.span_ $ fromFoldable $ map renderTablatureElem line <> HH.text "\n" : Nil
  renderTablatureElem (Prefix string) = HH.span [ classString "tabPrefix" ] [ HH.text string ]
  renderTablatureElem (Suffix string) = HH.span [ classString "tabSuffix" ] [ HH.text string ]
  renderTablatureElem (Timeline string) = HH.span [ classString "tabTimeline" ] [ HH.text string ]
  renderTablatureElem (Fret n) =
    if dozenalString == "↊" || dozenalString == "↋"
    then HH.span_ [ fretHtml, HH.span [ classString "tabTimeline" ] [ HH.text "-" ] ]
    else fretHtml
    where
    fretHtml = HH.span [ classString "tabFret" ] [ HH.text $ dozenalString ]
    dozenalString = toDozenalString n
  renderTablatureElem (Special string) = HH.span [ classString "tabSpecial" ] [ HH.text string ]

toDozenalString :: Int -> String
toDozenalString n = toStringAs dozenal n # replaceAll (Pattern "a") (Replacement "↊") # replaceAll (Pattern "b") (Replacement "↋")
  where
  dozenal = fromMaybe decimal $ radix 12
