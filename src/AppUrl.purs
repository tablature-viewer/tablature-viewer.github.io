module AppUrl where

import Prelude

import AppState (Action, State, Transposition(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console as Console
import Halogen as H
import LZString (compressToEncodedURIComponent, decompressFromEncodedURIComponent)
import LocationString (getFragmentString, getQueryParam, setFragmentString, setLocationString, setQueryString)

saveTablatureToUrl :: forall output m. MonadEffect m => H.HalogenM State Action () output m Unit
saveTablatureToUrl = do
  state <- H.get
  case compressToEncodedURIComponent state.tablatureText of
    Just compressed -> H.liftEffect $ setFragmentString compressed
    Nothing -> H.liftEffect $ Console.error("Could not save tablature to URL")

getTablatureTextFromUrl :: Effect (Maybe String)
getTablatureTextFromUrl = do
  fragment <- H.liftEffect getFragmentString
  if fragment == "" || fragment == "#"
  then pure Nothing
  else case decompressFromEncodedURIComponent fragment of
    Just decompressed -> pure $ Just decompressed
    Nothing -> Console.error("Could not load tablature from URL") *> pure Nothing

redirectToUrlInFragment :: Effect Unit
redirectToUrlInFragment = do
  compressedUrl <- getFragmentString
  case decompressFromEncodedURIComponent compressedUrl of
    Just url -> setLocationString url
    _ -> Console.error("Could not load decompressed shortlink URL") *> pure unit

getTranspositionFromUrl :: Effect (Maybe Transposition)
getTranspositionFromUrl  = do
  maybeTransposition <- getQueryParam "t"
  case maybeTransposition of
    Nothing -> pure Nothing
    Just transposition ->
      case fromString transposition of
        Just n -> pure $ Just $ Transposition n
        _ -> Console.error("Could not load decompressed shortlink URL") *> pure Nothing

setAppQueryString :: State -> Effect Unit
setAppQueryString state =
  case state.transposition of
    Transposition 0 -> setQueryString ""
    Transposition n -> setQueryString $ "t=" <> show n
