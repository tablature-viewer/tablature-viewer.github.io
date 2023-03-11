module CorsProxy where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AX
import AppState (Url)
import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import JSURI (encodeURIComponent)

fetchThroughProxy :: forall m. MonadAff m => Url -> MaybeT m String
fetchThroughProxy url = MaybeT do
  case encodeURIComponent url of
    Nothing -> do
      liftEffect $ Console.error $ "Could not encode " <> url
      pure Nothing
    Just encodedUrl -> do
      let corsUrl = "https://api.codetabs.com/v1/proxy?quest=" <> encodedUrl
      response <- liftAff $ AX.get ResponseFormat.string corsUrl
      liftEffect case response of
        Left err -> do
          Console.error $ "CORS Proxy response failed to decode: " <> AX.printError err
          pure Nothing
        Right result -> pure $ Just result.body
