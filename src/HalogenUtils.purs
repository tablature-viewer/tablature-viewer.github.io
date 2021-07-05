module HalogenUtils where

import Prelude

import Data.String (Pattern(..), split)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

classString :: forall t228 t229.  String -> HH.IProp ( class :: String | t228) t229
classString string = HP.classes $ split (Pattern " ") string <#> \s -> HH.ClassName s

fontAwesome :: forall w i. String -> HH.HTML w i
fontAwesome glyphName = HH.i [ classString $ "fas " <> glyphName] []

optionalText :: forall w i. String -> HH.HTML w i
optionalText text = HH.span [ classString "largeViewport" ] [ HH.text text ]

optionalHtml :: forall w i. HH.HTML w i -> HH.HTML w i
optionalHtml html = HH.span [ classString "largeViewport" ] [ html ]

alternativeHtml :: forall w i.  HH.HTML w i -> HH.HTML w i -> Array (HH.HTML w i)
alternativeHtml longHtml shortAlt = 
  [ HH.span [ classString "largeViewport" ] [ longHtml ]
  , HH.span [ classString "smallViewport" ] [ shortAlt ]
  ]

renderLineEnding :: forall w i. HH.HTML w i
renderLineEnding = HH.text "\n"
