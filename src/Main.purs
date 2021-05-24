module Main where

import Prelude
import QueryString
import Web.DOM.NonElementParentNode
import Web.HTML
import Web.HTML.HTMLTextAreaElement
import Web.HTML.Window

import Data.Maybe (Maybe(..))
import Data.String (drop, take)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Random (random)
import Halogen (Component)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.Element (Element, setAttribute)
import Web.HTML.HTMLDocument (toDocument)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type State = String

data Action = Save

component :: forall query input output m. MonadEffect m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ = ""

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.button [ HE.onClick \_ -> Save ] [ HH.text "Freeze" ]
    , HH.text (state)
    ]

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Save -> do
    string <- H.liftEffect getTablature
    H.liftEffect $ setRawQueryString string


getText :: Maybe Element -> Effect String
getText maybeElement = 
  let maybeText = (map value (maybeElement >>= fromElement)) 
  in case maybeText of
    Nothing -> pure ""
    Just s -> s

getTablature :: Effect String
getTablature = do
  w <- window
  d <- document w
  maybeElement <- getElementById "tablature_textarea" $ toNonElementParentNode $ toDocument d
  getText maybeElement
