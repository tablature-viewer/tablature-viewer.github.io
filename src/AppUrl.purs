module AppUrl where

import Prelude

import TablatureDocument (Transposition(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console as Console
import Halogen as H
import LZString (compressToEncodedURIComponent, decompressFromEncodedURIComponent)
import LocationString (getFragmentString, getQueryParam, setFragmentString, setLocationString, setQueryString)

type UrlState =
  { tablatureText :: String
  , transposition :: Transposition}

saveTablatureToUrl :: String -> Effect Unit
saveTablatureToUrl tablatureText = do
  case compressToEncodedURIComponent tablatureText of
    Just compressed -> H.liftEffect $ setFragmentString compressed
    Nothing -> H.liftEffect $ Console.error("Could not save tablature to URL")

getTablatureTextFromUrl :: Effect String
getTablatureTextFromUrl = do
  fragment <- H.liftEffect getFragmentString
  if fragment == "" || fragment == "#"
  then pure ""
  else case decompressFromEncodedURIComponent fragment of
    Just decompressed -> pure decompressed
    Nothing -> Console.error("Could not load tablature from URL") *> pure ""

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

setAppQueryString :: UrlState -> Effect Unit
setAppQueryString state =
  case state.transposition of
    Transposition 0 -> setQueryString ""
    Transposition n -> setQueryString $ "t=" <> show n
