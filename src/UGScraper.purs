module UGScraper where

import Prelude
import AppState

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Cache as Cache
import Clipboard (copyToClipboard)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.State (class MonadState)
import Data.Argonaut.Core (Json, caseJsonArray, caseJsonNumber, caseJsonObject, caseJsonString)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Enum (pred, succ)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String.Regex as Regex
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Debug (spy)
import DebugUtils (debugM_)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Foreign.Object (lookup)
import LocationString (getLocationString)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)
import TablatureDocument (predTransposition, succTransposition)
import TablatureRewriter (NoteOrientation)
import UrlShortener (createShortUrl)
import Web.DOM.DOMParser (makeDOMParser, parseHTMLFromString)
import Web.DOM.Document (toParentNode)
import Web.DOM.Element (getAttribute)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event as Event
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent as UIEvent

fetchTabFromUrl :: forall m. MonadAff m => MonadState State m => Url -> m (Maybe String)
fetchTabFromUrl url = runMaybeT do
  dataContent <- fetchUrlDataContent url
  MaybeT $ liftAff $ pure $ extractTab dataContent

execSearch :: forall m. MonadAff m => MonadState State m => String -> m Unit
execSearch phrase = map (const unit) $ runMaybeT do
  let url = """https://www.ultimate-guitar.com/search.php?search_type=title&value=""" <> phrase
  dataContent <- fetchUrlDataContent url
  searchResults <- MaybeT $ liftAff $ pure $ extractSearchResults dataContent
  setState _searchResults (Just $ filterSearchResults searchResults)
  pure unit

filterSearchResults :: Array SearchResult -> Array SearchResult
filterSearchResults = Array.filter pred
  where
  pred :: SearchResult -> Boolean
  pred { rating, contentType, marketingType } =
    Maybe.isJust rating
      && Maybe.isNothing marketingType
      && not Array.elem contentType [ Just "Pro", Just "Video", Just "Power" ]

extractSearchResults :: Json -> Maybe (Array SearchResult)
extractSearchResults input = do
  input # child "store" >>= child "page" >>= child "data" >>= child "results" >>= array <#> mapMaybe toSearchResult
  where
  -- TODO: move to json utils
  child :: String -> Json -> Maybe Json
  child name json = caseJsonObject Nothing (lookup name) json

  array :: Json -> Maybe (Array Json)
  array json = caseJsonArray Nothing Just json

  string :: Json -> Maybe String
  string json = caseJsonString Nothing Just json

  number :: Json -> Maybe Number
  number json = caseJsonNumber Nothing Just json

  toSearchResult :: Json -> Maybe SearchResult
  toSearchResult json =
    case
      do
        url <- child "tab_url" json >>= string
        name <- child "song_name" json >>= string
        artist <- child "artist_name" json >>= string
        let rating = child "rating" json >>= number
        let contentType = child "type" json >>= string
        let marketingType = child "marketing_type" json >>= string
        Just
          { url
          , rating
          , name
          , artist
          , contentType
          , marketingType
          }
      of
      Nothing -> do
        _ <- Just $ spy "Could not parse " json
        Nothing
      x -> do
        _ <- Just $ spy " " json
        x

fetchUrlDataContent :: forall m. MonadAff m => Url -> MaybeT m Json
fetchUrlDataContent url = do
  result <- MaybeT $ fetchThroughProxy url
  extractDataContent result

fetchThroughProxy :: forall m. MonadAff m => Url -> m (Maybe String)
fetchThroughProxy url = do
  let corsUrl = "https://api.codetabs.com/v1/proxy?quest=" <> url
  response <- liftAff $ AX.get ResponseFormat.string corsUrl
  liftEffect case response of
    Left err -> do
      Console.error $ "GET UG response failed to decode: " <> AX.printError err
      pure Nothing
    Right result -> pure $ Just result.body

extractDataContent :: forall m. MonadAff m => String -> MaybeT m Json
extractDataContent input = do
  parser <- liftEffect makeDOMParser
  doc <- MaybeT $ liftEffect $ hush <$> parseHTMLFromString input parser
  elem <- MaybeT $ liftEffect $ querySelector (QuerySelector ".js-store") (toParentNode doc)
  jsonString <- MaybeT $ liftEffect $ getAttribute "data-content" elem
  MaybeT $ pure $ hush $ jsonParser jsonString

extractTab :: Json -> Maybe String
extractTab input = do
  rawTab <- input # child "store" >>= child "page" >>= child "data" >>= child "tab_view" >>= child "wiki_tab" >>= child "content" >>= string
  pure $ Regex.replace (unsafeRegex """\[\/?(ch|tab)\]""" global) "" rawTab
  where
  child :: String -> Json -> Maybe Json
  child name json = caseJsonObject Nothing (\object -> lookup name object) json

  string :: Json -> Maybe String
  string json = caseJsonString Nothing Just json
