module UGScraper where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import AppState (SearchResult, State, Url, _searchResults, setState)
import Control.Error.Util as Error
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.State (class MonadState)
import Control.Monad.Writer (WriterT(..), runWriterT, tell)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String.Regex as Regex
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import JsonUtils (array, child, number, string)
import Utils (mapMaybeT)
import Web.DOM.DOMParser (makeDOMParser, parseHTMLFromString)
import Web.DOM.Document (toParentNode)
import Web.DOM.Element (getAttribute)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)

type LogMaybeT m a = MaybeT (WriterT (Array String) m) a

hoistMaybe' :: forall m a. Monad m => Maybe a -> LogMaybeT m a
hoistMaybe' maybe = MaybeT $ WriterT $ pure $ case maybe of
  Nothing -> Tuple Nothing mempty
  Just a -> Tuple (Just a) mempty

hoistMaybeT' :: forall m a. Monad m => MaybeT m a -> LogMaybeT m a
hoistMaybeT' maybeT = MaybeT $ WriterT $ do
  maybeA <- runMaybeT maybeT
  case maybeA of
    Nothing -> pure (Tuple Nothing mempty)
    Just a -> pure (Tuple (Just a) mempty)

foreign import _htmlDecode :: String -> String

execSearch :: forall m. MonadAff m => MonadState State m => String -> m Unit
execSearch phrase = map (const unit) $ runMaybeT do
  let url = """https://www.ultimate-guitar.com/search.php?search_type=title&value=""" <> phrase
  dataContent <- fetchUrlDataContent url
  (Tuple maybeSearchResults log) <- runWriterT $ runMaybeT $ extractSearchResults dataContent
  _ <- liftEffect $ traverse Console.error log
  searchResults <- Error.hoistMaybe maybeSearchResults
  setState _searchResults (Just $ filterSearchResults searchResults)

filterSearchResults :: Array SearchResult -> Array SearchResult
filterSearchResults = Array.filter pred
  where
  pred :: SearchResult -> Boolean
  pred { rating, contentType, marketingType } =
    Maybe.isJust rating
      && Maybe.isNothing marketingType
      && not Array.elem contentType [ Just "Pro", Just "Video", Just "Power" ]

extractSearchResults :: forall m. Monad m => Json -> LogMaybeT m (Array SearchResult)
extractSearchResults json = do
  let maybeJsonSearchResults = json # child "store" >>= child "page" >>= child "data" >>= child "results" >>= array
  when (Maybe.isNothing maybeJsonSearchResults) $ tell $ [ "Could not find search results in json " <> stringify json ]
  jsonSearchResults <- hoistMaybe' maybeJsonSearchResults
  MaybeT $ mapMaybeT toSearchResult jsonSearchResults <#> Just -- TODO: this looks not so streamlined

toSearchResult :: forall m. Monad m => Json -> LogMaybeT m SearchResult
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
    do
      when (Maybe.isNothing maybeResult) $ tell $ [ "Could not find search results in json " <> stringify json ]
      hoistMaybe' maybeResult

fetchTabFromUrl :: forall m. MonadAff m => MonadState State m => Url -> MaybeT m String
fetchTabFromUrl url = do
  dataContent <- fetchUrlDataContent url
  (Tuple maybeSearchResults log) <- runWriterT $ runMaybeT $ extractTab dataContent
  _ <- liftEffect $ traverse Console.error log
  Error.hoistMaybe maybeSearchResults

extractTab :: forall m. Monad m => Json -> LogMaybeT m String
extractTab json = do
  let maybeRawTab = json # child "store" >>= child "page" >>= child "data" >>= child "tab_view" >>= child "wiki_tab" >>= child "content" >>= string
  when (Maybe.isNothing maybeRawTab) $ tell [ "Could not retrieve tablature data from json " <> stringify json ]
  rawTab <- MaybeT $ pure $ maybeRawTab
  pure $ _htmlDecode $ Regex.replace (unsafeRegex """\[\/?(ch|tab)\]""" global) "" rawTab

fetchUrlDataContent :: forall m. MonadAff m => Url -> MaybeT m Json
fetchUrlDataContent url = do
  result <- fetchThroughProxy url
  extractDataContent result

fetchThroughProxy :: forall m. MonadAff m => Url -> MaybeT m String
fetchThroughProxy url = MaybeT do
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
