{-# LANGUAGE OverloadedStrings #-}
module Network.Typesense 
  ( mkClient
  , createCollection
  , retrieveCollection
  , listAllCollections
  , dropCollection
  , insertDocument
  , upsertDocument
  , searchDocuments
  , module Network.Typesense.Types
  ) where
import Control.Monad.Catch
import Data.Aeson
import qualified Data.Binary.Builder as B
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import Network.Typesense.Types
import Network.HTTP.Client
    ( parseUrlThrow, Request(requestHeaders) )
import Network.HTTP.Simple
import qualified Data.Text.Encoding as T
import Network.HTTP.Types
import Web.HttpApiData

withPathAndMethod :: Method -> [Text] -> Client -> Client
withPathAndMethod m p (Client c) = 
  Client $
  setRequestMethod m $
  setRequestPath (L.toStrict $ B.toLazyByteString $ encodePathSegments p) c

withJSONBody :: ToJSON a => a -> Client -> Client
withJSONBody x (Client c) = Client $ setRequestBodyJSON x c

withQueryString :: Query -> Client -> Client
withQueryString q (Client c) = Client $ setRequestQueryString q c

mkClient :: MonadThrow m => String -> ApiKey -> m Client
mkClient url (ApiKey k) = do
  u <- parseUrlThrow url
  let u' = u { requestHeaders = ("X-TYPESENSE-API-KEY", T.encodeUtf8 k) : requestHeaders u }
  pure $ Client u'

createCollection :: (MonadTypesense m) => CollectionSchema -> m Collection
createCollection s = do 
  c <- askTypesenseClient
  fmap getResponseBody $ 
    httpJSON $ 
    typesenseRequest $ 
    withPathAndMethod methodPost ["collections"] $ 
    withJSONBody s c

-- TODO something better than just text as param
retrieveCollection :: (MonadTypesense m) => CollectionName a -> m Collection
retrieveCollection n = do 
  c <- askTypesenseClient 
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodGet ["collections", toUrlPiece n] c

listAllCollections :: (MonadTypesense m) => m [Collection]
listAllCollections = do
  c <- askTypesenseClient 
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodGet ["collections"] c

dropCollection :: (MonadTypesense m) => CollectionName a -> m Collection
dropCollection n = do 
  c <- askTypesenseClient
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodDelete ["collections", toUrlPiece n] c

insertDocument :: (MonadTypesense m, ToJSON a, FromJSON a) => CollectionName a -> a -> m (Document a)
insertDocument n x = do
  c <- askTypesenseClient
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodPost ["collections", toUrlPiece n, "documents"] $
    withJSONBody x c

upsertDocument :: (MonadTypesense m, ToJSON a, FromJSON a) => CollectionName a -> a -> m (Document a)
upsertDocument n x = do
  c <- askTypesenseClient
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodPost ["collections", toUrlPiece n, "documents"] $
    withQueryString [("action", Just "upsert")] $
    withJSONBody x c

-- Note that it's up to the caller to ensure that the type
searchDocuments :: (MonadTypesense m, FromJSON b) => CollectionName a -> SearchQuery -> m (SearchResult b)
searchDocuments n q = do
  c <- askTypesenseClient
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withQueryString (searchQueryToParams q) $
    withPathAndMethod methodGet ["collections", toUrlPiece n, "documents", "search"] c