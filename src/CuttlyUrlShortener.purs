module CuttlyUrlShortener where

import Prelude

import AppState (Url)
import Control.Monad.Maybe.Trans (MaybeT(..))
import CorsProxy (fetchThroughProxy)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import JSURI (encodeURIComponent)
import LZString (compressToEncodedURIComponent)
import LocationString (getLocationBaseString)
import Simple.JSON (readJSON)

type RequestBodyRecord = { url :: String }
type ResponseRecord = { url :: { status :: Int } }
type SuccessResponseRecord = { url :: { shortLink :: String } }

createShortUrl :: forall m. MonadAff m => Url -> MaybeT m String
createShortUrl url = do
  encodedUrl <- MaybeT $ pure $ encodeURIComponent url
  result <- fetchThroughProxy $ "https://cutt.ly/api/api.php?key=d27fec420c4daade4093757967023d6b70bc7&short=" <> encodedUrl
  case readJSON result of
    Right ({ url: { shortLink } } :: SuccessResponseRecord) -> createCustomShortUrl shortLink
    Left _ ->
      case readJSON result of
        Right ({ url: { status } } :: ResponseRecord) -> do
          liftEffect (Console.error $ "Received error status from cuttly API: " <> show status)
          MaybeT (pure Nothing)
        Left e -> do
          liftEffect $ Console.error $ "Can't parse response JSON: " <> show e <> ". Response: " <> result
          MaybeT (pure Nothing)

createCustomShortUrl :: forall m. MonadAff m => Url -> MaybeT m String
createCustomShortUrl shortUrl = do
  base <- liftEffect getLocationBaseString
  case compressToEncodedURIComponent shortUrl of
    Just compressed -> pure $ base <> "?u#" <> compressed
    _ -> liftEffect (Console.error "Could not compress shortlink URI") *> MaybeT (pure Nothing)
