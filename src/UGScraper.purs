module UGScraper where

import JsonUtils
import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import AppState (SearchResult, State, Url, _searchResults, setState)
import Control.Error.Util (hoistMaybe)
import Control.Monad.Cont (lift)
import Control.Monad.Except (ExceptT(..), except, runExcept)
import Control.Monad.List.Trans (scanl)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.State (class MonadState)
import Data.Argonaut.Core (Json, caseJsonArray, caseJsonNumber, caseJsonObject, caseJsonString, stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Either (Either(..), hush, note)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String.Regex as Regex
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Traversable, traverse)
import Debug (spy)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Foreign.Object (lookup)
import Web.DOM.DOMParser (makeDOMParser, parseHTMLFromString)
import Web.DOM.Document (toParentNode)
import Web.DOM.Element (getAttribute)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)

fetchTabFromUrl :: forall m. MonadAff m => MonadState State m => Url -> MaybeT m String
fetchTabFromUrl url = do
  dataContent <- fetchUrlDataContent url
  extractTab dataContent

execSearch :: forall m. MonadAff m => MonadState State m => String -> m Unit
execSearch phrase = map (const unit) $ runMaybeT do
  let url = """https://www.ultimate-guitar.com/search.php?search_type=title&value=""" <> phrase
  dataContent <- fetchUrlDataContent url
  searchResults <- extractSearchResults dataContent
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

-- traverse in the base monad, then filter the results
mapMaybeT :: forall m a b. Monad m => (a -> MaybeT m b) -> Array a -> m (Array b)
mapMaybeT f array = do
  maybes <- traverse (runMaybeT <<< f) array
  values <- pure $ mapMaybe identity maybes
  pure values

extractSearchResults :: forall m. MonadEffect m => Json -> MaybeT m (Array SearchResult)
extractSearchResults json = do
  jsonSearchResults <- json # child "store" >>= child "page" >>= child "data" >>= child "results" >>= array # hoistMaybe
  -- when (Maybe.isNothing jsonSearchResults) $ liftEffect $ Console.error $ "Could not retrieve data from json " <> stringify json
  -- FIXNOW: this is wrong, traverse will not filter but stop on the first Nothing
  mapMaybeT toSearchResult jsonSearchResults

toSearchResult :: forall m. MonadEffect m => Json -> MaybeT m SearchResult
toSearchResult json =
  let
    maybeResult = do
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
  in
    MaybeT case maybeResult of
      Nothing -> do
        liftEffect $ Console.error $ "Could not parse " <> stringify json
        pure Nothing
      justResult -> do
        -- _ <- Just $ spy " " json
        pure justResult

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

extractTab :: forall m. MonadEffect m => Json -> MaybeT m String
extractTab json = do
  rawTab <- json # child "store" >>= child "page" >>= child "data" >>= child "tab_view" >>= child "wiki_tab" >>= child "content" >>= string # hoistMaybe
  -- when (Maybe.isNothing rawTab) $ liftEffect $ Console.error $ "Could not retrieve tablature data from json " <> stringify json
  pure $ Regex.replace (unsafeRegex """\[\/?(ch|tab)\]""" global) "" rawTab
