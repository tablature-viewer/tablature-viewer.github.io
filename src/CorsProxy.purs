module CorsProxy where

import Prelude

import Affjax.Web as AX
import Affjax.ResponseFormat as ResponseFormat
import AppState (Url)
import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console

fetchThroughProxy :: forall m. MonadAff m => Url -> MaybeT m String
fetchThroughProxy url = MaybeT do
  let corsUrl = "https://api.codetabs.com/v1/proxy?quest=" <> url
  response <- liftAff $ AX.get ResponseFormat.string corsUrl
  liftEffect case response of
    Left err -> do
      Console.error $ "CORS Proxy response failed to decode: " <> AX.printError err
      pure Nothing
    Right result -> pure $ Just result.body
