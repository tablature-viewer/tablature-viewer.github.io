module UrlShortener where

import Prelude

import Affjax (defaultRequest, request)
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (ResponseFormat)
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Data.HTTP.Method as Method
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Simple.JSON (readJSON, writeJSON)

type RequestRecord = {action :: String, format :: String, url :: String, title :: String}
type ResponseRecord = {shorturl :: String, message :: String}

instance showR :: forall a. Show (ResponseFormat a) where
  show _ = ""

createShortUrl :: String -> String -> Aff (Maybe String)
createShortUrl url title = do
  liftEffect $ Console.log $ show req
  response <- request req
  case response of
    Right result -> do
      liftEffect $ Console.log result.body
      case readJSON result.body of
        Right (r :: ResponseRecord) -> do
          liftEffect $ Console.log r.message
          pure $ Just $ r.shorturl
        Left e -> do
          liftEffect $ Console.error $ "Can't parse response JSON: " <> show e
          pure Nothing
    _ -> liftEffect $ Console.error "No valid response obtained." *> pure Nothing
  where
  apiEndpoint = "https://tny.im/yourls-api.php"
  reqBody = { action: "shorturl", format: "json", url: url, title: title } :: RequestRecord
  req = defaultRequest
    { url = apiEndpoint
    , method = Left Method.POST
    , headers = []
    , content = Just $ RequestBody.string $ writeJSON reqBody
    , responseFormat = ResponseFormat.string}
