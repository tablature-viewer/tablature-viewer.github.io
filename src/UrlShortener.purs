module UrlShortener where

import Prelude

import Affjax (defaultRequest, request)
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Data.HTTP.Method as Method
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Simple.JSON (readJSON, writeJSON)

type RequestBodyRecord = {url :: String}
type ResponseRecord = {shortlink :: {shortlink :: String}}

createShortUrl :: String -> Aff (Maybe String)
createShortUrl url = do
  response <- request req
  case response of
    Right result -> do
      case readJSON result.body of
        Right (r :: ResponseRecord) -> do
          pure $ Just $ r.shortlink.shortlink
        Left e -> do
          liftEffect $ Console.error $ "Can't parse response JSON: " <> show e
          pure Nothing
    _ -> liftEffect $ Console.error "No valid response obtained." *> pure Nothing
  where
  apiEndpoint = "https://shortlink1.p.rapidapi.com/"
  reqBody = { url: url }
  req = defaultRequest
    { url = apiEndpoint
    , method = Left Method.POST
    , headers =
      [ RequestHeader "content-type" "application/json"
      , RequestHeader "x-rapidapi-key" "5dffbe9520msh2ca951c0dbb64cdp1aebe4jsn61b4bad6925a"
      , RequestHeader "x-rapidapi-host" "shortlink1.p.rapidapi.com"
      , RequestHeader "useQueryString" "true"
      ]
    , content = Just $ RequestBody.string $ writeJSON reqBody
    , responseFormat = ResponseFormat.string}
