module AppUrl where

import Prelude

import AppState (Action, State)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console as Console
import Halogen as H
import LZString (compressToEncodedURIComponent, decompressFromEncodedURIComponent)
import LocationString (getFragmentString, setFragmentString, setQueryString)

saveTablatureToUrl :: forall output m. MonadEffect m => H.HalogenM State Action () output m Unit
saveTablatureToUrl = do
  state <- H.get
  case compressToEncodedURIComponent state.tablatureText of
    Just compressed -> H.liftEffect $ setFragmentString compressed
    Nothing -> H.liftEffect $ Console.error("Could not save tablature to URL")
  -- Add a version string for future compatibility
  H.liftEffect $ setQueryString "v=1"

getTablatureTextFromUrl :: Effect (Maybe String)
getTablatureTextFromUrl = do
  fragment <- H.liftEffect getFragmentString
  if fragment == "" || fragment == "#"
  then pure Nothing
  else case decompressFromEncodedURIComponent fragment of
    Just decompressed -> pure $ Just decompressed
    Nothing -> Console.error("Could not load tablature from URL") *> (pure Nothing)
