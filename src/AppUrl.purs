module AppUrl where

import Prelude

import Data.Array (mapMaybe)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console as Console
import Halogen as H
import LZString (compressToEncodedURIComponent, decompressFromEncodedURIComponent)
import LocationString (getFragmentString, getQueryParam, setFragmentString, setLocationString, setQueryComponents)
import TablatureDocument (Transposition(..), identityTransposition)
import TablatureRewriter (NoteOrientation(..))

type UrlParams =
  { transposition :: Transposition
  , noteOrientation :: NoteOrientation
  }

saveTablatureToUrl :: String -> Effect Unit
saveTablatureToUrl tablatureText = do
  case compressToEncodedURIComponent tablatureText of
    Just compressed -> H.liftEffect $ setFragmentString compressed
    Nothing -> H.liftEffect $ Console.error ("Could not save tablature to URL")

getTablatureTextFromUrl :: Effect String
getTablatureTextFromUrl = do
  fragment <- H.liftEffect getFragmentString
  if fragment == "" || fragment == "#" then pure ""
  else case decompressFromEncodedURIComponent fragment of
    Just decompressed -> pure decompressed
    Nothing -> Console.error ("Could not load tablature from URL") *> pure ""

redirectToUrlInFragment :: Effect Unit
redirectToUrlInFragment = do
  compressedUrl <- getFragmentString
  case decompressFromEncodedURIComponent compressedUrl of
    Just url -> setLocationString url
    _ -> Console.error ("Could not load decompressed shortlink URL") *> pure unit

getTranspositionFromUrl :: Effect (Maybe Transposition)
getTranspositionFromUrl = do
  maybeTransposition <- getQueryParam "t"
  case maybeTransposition of
    Nothing -> pure Nothing
    Just transposition ->
      case fromString transposition of
        Just n -> pure $ Just $ Transposition n
        _ -> Console.error ("Could not parse transposition parameter") *> pure Nothing

getNoteOrientationFromUrl :: Effect (Maybe NoteOrientation)
getNoteOrientationFromUrl = do
  maybeNoteOrientation <- getQueryParam "o"
  case maybeNoteOrientation of
    Nothing -> pure Nothing
    Just noteOrientation ->
      case fromString noteOrientation of
        Just 0 -> pure $ Just Flat
        Just 1 -> pure $ Just Sharp
        _ -> Console.error ("Could not parse note orientation parameter") *> pure Nothing

getAppUrlParams :: Effect UrlParams
getAppUrlParams = do
  maybeTransposition <- getTranspositionFromUrl
  maybeNoteOrientation <- getNoteOrientationFromUrl
  pure $
    { transposition: fromMaybe identityTransposition maybeTransposition
    , noteOrientation: fromMaybe Default maybeNoteOrientation
    }

setAppQueryString :: UrlParams -> Effect Unit
setAppQueryString params = setQueryComponents $ mapMaybe identity
  [ case params.noteOrientation of
      Default -> Nothing
      Flat -> Just "o=0"
      Sharp -> Just "o=1"
  , case params.transposition of
      Transposition 0 -> Nothing
      Transposition n -> Just $ "t=" <> show n
  ]

resetUrlParams :: Effect Unit
resetUrlParams = setQueryComponents []
