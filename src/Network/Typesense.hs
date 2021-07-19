{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}
{-|
Module      : Network.Typesense
Description : Haskell client 
Copyright   : (c) Ian Duncan, 2021
License     : BSD-3
Maintainer  : ian@iankduncan.com
Stability   : experimental
Portability : POSIX

Typesense is an open-source, typo-tolerant search engine optimized for instant (typically sub-50ms) search-as-you-type experiences and developer productivity.

If you've heard about ElasticSearch or Algolia, a good way to think about Typesense is that it is:

An open source alternative to Algolia, with some key quirks solved and
An easier-to-use batteries-included alternative to ElasticSearch
🗣️ 🎥 If you prefer watching videos, here's one where we introduce Typesense and show a walk-through: https://youtu.be/F4mB0x_B1AE?t=144
-}
module Network.Typesense
  (
  -- * Initializing the client
    mkClient
  -- * Collections
  , createCollection
  , retrieveCollection
  , listAllCollections
  , dropCollection
  -- * Documents
  , insertDocument
  , upsertDocument
  -- retrieveDocument
  -- TODO
  -- , updateDocument
  -- ** Batch document insertion
  , importDocuments
  , ImportMode(..)
  -- TODO foldable/traverseable utility version
  -- ** Search
  , searchDocuments
  -- TODO multisearch
  -- *** Optional search parameters
  -- | The relative weight to give each query_by field when ranking results. This can be used to boost fields in priority, when looking for matches.
  -- Fields should be in the same order as the queryBy fields. For example: query_by_weights: 1,1,2 with query_by: field_a,field_b,field_c will give equal weightage to field_a and field_b, and will give twice the weightage to field_c comparatively.
  , queryByWeights 
  -- | Indicates that the last word in the query should be treated as a prefix, and not as a whole word. This is necessary for building autocomplete and instant search interfaces. Set this to false to disable prefix searching for all queried fields.
  --
  -- You can control the behavior of prefix search on a per field basis. For example, if you are querying 3 fields and want to enable prefix searching only on the first field, use ?prefix=true,false,false. The order should match the order of fields in query_by.
  --
  -- Default: true (prefix searching is enabled for all fields).
  , prefix
  -- | Filter conditions for refining your search results.
  --
  -- A field can be matched against one or more values.
  --
  -- country: USA
  --
  -- country: [USA, UK] returns documents that have country of USA OR UK.
  --
  -- To match a string field exactly, you have to mark the field as a facet and use the := operator. For eg: category:= Shoe will match documents from the category shoes and not from a category like shoe rack.
  --
  -- You can also filter using multiple values and use the backtick character to denote a string literal: category:= [`Running Shoes, Men`, Sneaker].
  --
  -- Not equals / negation is supported for string and boolean facet fields, e.g. filter_by=author:!= JK Rowling
  --
  -- Get numeric values between a min and max value, using the range operator [min..max]
  --
  -- For eg: num_employees:[10..100]
  --
  -- Separate multiple conditions with the && operator.
  --
  -- For eg: num_employees:>100 && country: [USA, UK]
  --
  -- More examples:
  --
  -- num_employees:10
  -- num_employees:<=10
  , filterBy
  -- | A list of numerical fields and their corresponding sort orders that will be used for ordering your results. Separate multiple fields with a comma. Up to 3 sort fields can be specified.
  --
  -- E.g. num_employees:desc,year_started:asc
  --
  -- The text similarity score is exposed as a special _text_match field that you can use in the list of sorting fields.
  --
  -- If one or two sorting fields are specified, _text_match is used for tie breaking, as the last sorting field.
  --
  -- Default:
  --
  -- If no sort_by parameter is specified, results are sorted by: _text_match:desc,default_sorting_field:desc.
  , sortBy
  -- | A list of fields that will be used for faceting your results on. Separate multiple fields with a comma.
  , facetBy
  -- | Maximum number of facet values to be returned.
  , maxFacetValues
  -- | Facet values that are returned can now be filtered via this parameter. The matching facet text is also highlighted. For example, when faceting by category, you can set facet_query=category:shoe to return only facet values that contain the prefix "shoe".
  , facetQuery
  -- | By default, Typesense prioritizes documents whose field value matches exactly with the query. Set this parameter to false to disable this behavior.
  --
  -- Default: true
  , prioritizeExactMatch 
  -- | Results from this specific page number would be fetched.
  , page
  -- | Number of results to fetch per page.
  --
  -- Default: 10
  --
  -- NOTE: Only upto 250 hits can be fetched per page.
  , perPage
  -- | You can aggregate search results into groups or buckets by specify one or more group_by fields. Separate multiple fields with a comma.
  --
  -- NOTE: To group on a particular field, it must be a faceted field.
  --
  -- E.g. group_by=country,company_name
  , groupBy
  -- | Maximum number of hits to be returned for every group. If the group_limit is set as K then only the top K hits in each group are returned in the response.
  --
  -- Default: 3
  , groupLimit
  -- | Comma-separated list of fields from the document to include in the search result.
  , includeFields
  -- | Comma-separated list of fields from the document to exclude in the search result.
  , excludeFields
  -- | Comma separated list of fields that should be highlighted with snippetting. You can use this parameter to highlight fields that you don't query for, as well.
  --
  -- Default: all queried fields will be highlighted.
  , highlightFields
  -- | Comma separated list of fields which should be highlighted fully without snippeting.
  --
  -- Default: all fields will be snippeted.
  , highlightFullFields 
  -- | The number of tokens that should surround the highlighted text on each side.
  --
  -- Default: 4
  , highlightAffixNumTokens 
  -- | The start tag used for the highlighted snippets.
  --
  -- Default: <mark> 
  , highlightStartTag
  -- | The end tag used for the highlighted snippets.
  --
  -- Default: </mark>
  , highlightEndTag
  -- | Field values under this length will be fully highlighted, instead of showing a snippet of relevant portion.
  --
  -- Default: 30
  , snippetThreshold
  -- | Maximum number of typographical errors (0, 1 or 2) that would be tolerated.
  --
  -- Damerau–Levenshtein distance is used to calculate the number of errors.
  --
  -- Default: 2
  , numTypos
  -- | If at least typo_tokens_threshold number of results are not found for a specific query, Typesense will attempt to look for results with more typos until num_typos is reached or enough results are found. Set typo_tokens_threshold to 0 to disable typo tolerance.
  --
  -- Default: 100
  , typoTokensThreshold 
  -- | If at least drop_tokens_threshold number of results are not found for a specific query, Typesense will attempt to drop tokens (words) in the query until enough results are found. Tokens that have the least individual hits are dropped first. Set drop_tokens_threshold to 0 to disable dropping of tokens.
  --
  -- Default: 10
  , dropTokensThreshold
  -- | A list of records to unconditionally include in the search results at specific positions.
  --
  -- An example use case would be to feature or promote certain items on the top of search results.
  -- 
  -- A comma separated list of record_id:hit_position. Eg: to include a record with ID 123 at Position 1 and another record with ID 456 at Position 5, you'd specify 123:1,456:5.
  -- 
  -- You could also use the Overrides feature to override search results based on rules. Overrides are applied first, followed by pinned_hits and finally hidden_hits.
  , pinnedHits
  -- | A list of records to unconditionally hide from search results.
  --
  -- A comma separated list of record_ids to hide. Eg: to hide records with IDs 123 and 456, you'd specify 123,456.
  --
  -- You could also use the Overrides feature to override search results based on rules. Overrides are applied first, followed by pinned_hits and finally hidden_hits.
  , hiddenHits
  -- | If you have some overrides defined but want to disable all of them for a particular search query, set enable_overrides to false.
  --
  -- Default: true
  , enableOverrides
  -- | Set this parameter to true if you wish to split the search query into space separated words yourself. When set to true, we will only split the search query by space, instead of using the locale-aware, built-in tokenizer.
  --
  -- Default: false
  , preSegmentedQuery
  -- | Maximum number of hits that can be fetched from the collection. Eg: 200
  --
  -- page * per_page should be less than this number for the search request to return results.
  --
  -- Default: no limit
  --
  -- You'd typically want to generate a scoped API key with this parameter embedded and use that API key to perform the search, so it's automatically applied and can't be changed at search time.
  , limitHits
  -- * API Keys
  , createApiKey
  , retrieveApiKey
  , listApiKeys
  , deleteApiKey
  , generateScopedApiKey
  -- Curation
  -- , createOrUpdateOverride
  -- , retrieveOverride
  -- , listOverrides
  -- , deleteOverride

  -- * Collection Aliases
  -- $collectionAliases
  , createOrUpdateAlias
  , retrieveAlias
  , listAllAliases
  , deleteAlias
  -- * Synonyms
  -- $synonyms
  , createOrUpdateSynonym
  , retrieveSynonym
  , listSynonyms
  , deleteSynonym
  -- * Cluster Operations
  , createSnapshot
  , reElectLeader
  -- TODO, this hits a base config endpoint:
  -- toggleSlowRequestLog
  , clusterMetrics 
  , apiStats
  , health
  -- ** Types
  , module Network.Typesense.Types
  ) where
import Control.Monad.Catch
import Data.Aeson
import qualified Data.Binary.Builder as B
import Data.ByteArray.Encoding
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import Network.Typesense.Types
import Network.Typesense.Types.Internal
import Network.HTTP.Client
    ( parseUrlThrow, Request(requestHeaders), RequestBody (RequestBodyStreamChunked), HttpExceptionContent (StatusCodeException) )
import Network.HTTP.Simple
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types
import Web.HttpApiData
import Conduit
import Crypto.MAC.HMAC
import qualified Data.Binary.Builder as Builder
import Network.HTTP.Client.Conduit (requestBody, responseStatus)
import qualified Data.ByteString as S
import qualified Network.HTTP.Client as HCC
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Monad.Trans.Resource (getInternalState, runInternalState)
import Crypto.Hash (SHA256)
import System.Posix.Types (EpochTime)
import qualified Data.ByteString as B

withPathAndMethod :: Method -> [Text] -> Client -> Client
withPathAndMethod m p (Client c) =
  Client $
  setRequestMethod m $
  setRequestPath (L.toStrict $ B.toLazyByteString $ encodePathSegments p) c

withJSONBody :: ToJSON a => a -> Client -> Client
withJSONBody x (Client c) = Client $ setRequestBodyJSON x c

withQueryString :: Query -> Client -> Client
withQueryString q (Client c) = Client $ setRequestQueryString q c

handle404 :: (MonadThrow m, MonadCatch m) => m (Response a) -> m (Response (Maybe a))
handle404 m = (fmap Just <$> m) `catch` \case
  HttpExceptionRequest req e -> case e of
    StatusCodeException resp firstSection -> case statusCode (responseStatus resp) of
      404 -> pure (Nothing <$ resp)
      n -> throwM $ HttpExceptionRequest req e
    otherErr -> throwM $ HttpExceptionRequest req e
  otherErr -> throwM otherErr


mkClient :: MonadThrow m => String -> ApiKey -> m Client
mkClient url (ApiKey k) = do
  u <- parseUrlThrow url
  let u' = u { requestHeaders = ("X-TYPESENSE-API-KEY", T.encodeUtf8 k) : requestHeaders u }
  pure $ Client u'

createCollection :: (MonadTypesense m) => CollectionSchema a -> m (Collection a)
createCollection s = do
  c <- askTypesenseClient
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodPost ["collections"] $
    withJSONBody s c

-- | Retrieve the details of a collection, given its name.
retrieveCollection :: (MonadTypesense m) => CollectionName a -> m (Maybe (Collection a))
retrieveCollection n = do
  c <- askTypesenseClient
  fmap getResponseBody $
    handle404 $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodGet ["collections", toUrlPiece n] c

-- | Returns a summary of all your collections. The collections are returned sorted by creation date, with the most recent collections appearing first.
listAllCollections :: (MonadTypesense m) => m [Collection a]
listAllCollections = do
  c <- askTypesenseClient
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodGet ["collections"] c

-- | Permanently drops a collection. This action cannot be undone. For large collections, this might have an impact on read latencies.
dropCollection :: (MonadTypesense m) => CollectionName a -> m (Collection a)
dropCollection n = do
  c <- askTypesenseClient
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodDelete ["collections", toUrlPiece n] c

-- | A document to be indexed in a given collection must conform to the schema of the collection.
-- 
-- If the document contains an id field of type string, Typesense would use that field as the identifier for the document. Otherwise, Typesense would assign an identifier of its choice to the document. Note that the id should not include spaces or any other characters that require encoding in urls (opens new window).
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

data ImportMode 
  = Create 
  -- ^ Creates a new document. Fails if a document with the same id already exists
  | Upsert 
  -- ^ Creates a new document or updates an existing document if a document with the same id already exists. Requires the whole document to be sent. For partial updates, use the update action below.
  | Update
  -- ^ Updates an existing document. Fails if a document with the given id does not exist. You can send a partial document containing only the fields that are to be updated.

-- | You can index multiple documents in a batch using the import API.
importDocuments :: (MonadTypesense m, ToJSON a) => CollectionName a -> ImportMode -> ConduitM () a (ResourceT IO) () -> m ()
importDocuments n mode m = do
  c <- askTypesenseClient
  fmap getResponseBody $
    httpNoBody $ -- TODO?
    (\req -> req { requestBody = requestBodySourceChunked producer }) $
    typesenseRequest $
    withQueryString
      [ ("action"
        , Just $! case mode of
            Create -> "create"
            Upsert -> "upsert"
            Update -> "update"
        )
      ] $
    withPathAndMethod methodPost ["collections", toUrlPiece n, "documents", "import"] c
  where
    producer = m .| jsonlOutput

jsonlOutput :: (PrimMonad m, ToJSON a) => ConduitM a B.ByteString m ()
jsonlOutput = Conduit.mapC (\x -> fromEncoding (toEncoding x) <> Builder.singleton 0x0A) .| builderToByteString

-- TODO http-conduit between 2.7 and 2.8 changed the type on requestBodySourceChunked,
-- so copy-pasting here to get the resourcet version
requestBodySourceChunked :: ConduitM () S.ByteString (ResourceT IO) () -> RequestBody
requestBodySourceChunked = RequestBodyStreamChunked . srcToPopper

srcToPopper :: ConduitM () S.ByteString (ResourceT IO) () -> HCC.GivesPopper ()
srcToPopper src f = runResourceT $ do
    (rsrc0, ()) <- src $$+ return ()
    irsrc <- liftIO $ newIORef rsrc0
    is <- getInternalState
    let popper :: IO S.ByteString
        popper = do
            rsrc <- readIORef irsrc
            (rsrc', mres) <- runInternalState (rsrc $$++ await) is
            writeIORef irsrc rsrc'
            case mres of
                Nothing -> return S.empty
                Just bs
                    | S.null bs -> popper
                    | otherwise -> return bs
    liftIO $ f popper

-- | Note that it's up to the caller to ensure that the type is right
searchDocuments :: (MonadTypesense m, FromJSON b) => CollectionName a -> SearchQuery -> m (SearchResult b)
searchDocuments n q = do
  c <- askTypesenseClient
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withQueryString (searchQueryToParams q) $
    withPathAndMethod methodGet ["collections", toUrlPiece n, "documents", "search"] c

-- $collectionAliases
-- An alias is a virtual collection name that points to a real collection. If you're familiar with symbolic links on Linux, it's very similar to that.
-- 
-- Aliases are useful when you want to reindex your data in the background on a new collection and switch your application to it without any changes to your code. Let's take an example.
--
-- Let's say we have a collection called companies_june10 and an alias called companies pointing to that collection.
--
-- collection ---> companies_june10
--
-- On the next day (June 11), we will create a new collection called companies_june11 and start indexing the documents in the background into this collection. When we are done indexing, if we updated the companies alias to point to this new collection, your application would immediately start querying against the freshly indexed collection.
--
-- collection ---> companies_june11

createOrUpdateAlias :: (MonadTypesense m) => Alias a -> m (Alias a)
createOrUpdateAlias (Alias n coll) = do
  c <- askTypesenseClient
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodPut ["aliases", toUrlPiece n] $
    withJSONBody (NewAlias coll) c

retrieveAlias :: (MonadTypesense m) => CollectionName a -> m (Maybe (Alias a))
retrieveAlias n = do
  c <- askTypesenseClient
  fmap getResponseBody $
    handle404 $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodGet ["aliases", toUrlPiece n] c

listAllAliases :: (MonadTypesense m) => m Aliases
listAllAliases = do
  c <- askTypesenseClient
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodGet ["aliases"] c

deleteAlias :: (MonadTypesense m) => CollectionName a -> m (Alias a)
deleteAlias n = do
  c <- askTypesenseClient
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodDelete ["aliases", toUrlPiece n] c

-- $synonyms
-- The synonyms feature allows you to define search terms that should be considered equivalent. For eg: when you define a synonym for sneaker as shoe, searching for sneaker will now return all records with the word shoe in them, in addition to records with the word sneaker.
--
-- Typesense supports two types of synonyms:
--
-- One-way synonyms: Defining the words iphone and android as one-way synonyms of smart phone will cause searches for smart phone to return documents containing iphone or android or both.
-- 
-- Multi-way synonyms: Defining the words blazer, coat and jacket as multi-way synonyms will cause searches for any one of those words (eg: coat) to return documents containing at least one of the words in the synonym set (eg: records with blazer or coat or jacket are returned).

createOrUpdateSynonym :: (MonadTypesense m) => CollectionName a -> Id Synonym -> Synonym -> m (Entity Synonym)
createOrUpdateSynonym coll synId syn = do
  c <- askTypesenseClient 
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodPut ["collections", toUrlPiece coll, "synonyms", toUrlPiece synId] $
    withJSONBody syn c

-- | We can retrieve a single synonym.
retrieveSynonym :: (MonadTypesense m) => CollectionName a -> Id Synonym -> m (Maybe (Entity Synonym))
retrieveSynonym coll synId = do
  c <- askTypesenseClient 
  fmap getResponseBody $
    handle404 $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodGet ["collections", toUrlPiece coll, "synonyms", toUrlPiece synId] c

-- | List all synonyms associated with a given collection.
listSynonyms :: (MonadTypesense m) => CollectionName a -> m SynonymsList
listSynonyms coll = do
  c <- askTypesenseClient 
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodGet ["collections", toUrlPiece coll, "synonyms"] c

-- | Delete a synonym associated with a collection.
deleteSynonym :: (MonadTypesense m) => CollectionName a -> Id Synonym -> m (Maybe (IdOnlyResponse Synonym))
deleteSynonym coll synId = do
  c <- askTypesenseClient 
  fmap getResponseBody $
    handle404 $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodDelete ["collections", toUrlPiece coll, "synonyms", toUrlPiece synId] c

createApiKey :: (MonadTypesense m) => NewAPIKey -> m CreatedAPIKey 
createApiKey x = do
  c <- askTypesenseClient
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodPost ["keys"] $
    withJSONBody x c

-- | Retrieve (metadata about) a key.
retrieveApiKey :: (MonadTypesense m) => Text -> m (Maybe ExistingAPIKey)
retrieveApiKey keyId = do
  c <- askTypesenseClient
  fmap getResponseBody $
    handle404 $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodGet ["keys", toUrlPiece keyId] c

-- | Retrieve (metadata about) all keys.
listApiKeys :: (MonadTypesense m) => m [ExistingAPIKey]
listApiKeys = do
  c <- askTypesenseClient
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodGet ["keys"] c

-- | Delete an API key given its ID.
deleteApiKey :: (MonadTypesense m) => Text -> m (Maybe DeletedAPIKey)
deleteApiKey keyId = do
  c <- askTypesenseClient
  fmap getResponseBody $
    handle404 $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodDelete ["keys", toUrlPiece keyId] c

generateScopedApiKey :: APIKey -> QueryParams -> APIKey
generateScopedApiKey (APIKey keyWithSearchPermissions) embeddedSearchParameters = APIKey scopedApiKey
  where
    encodedParameters = L.toStrict $ encode embeddedSearchParameters
    digest = convertToBase Base64 (hmac keyWithSearchPermissions encodedParameters :: HMAC SHA256)
    scopedApiKey = convertToBase Base64 (digest <> B.take 4 keyWithSearchPermissions <> encodedParameters)






-- | Creates a point-in-time snapshot of a Typesense node's state and data in the specified directory.
--
-- You can then backup the snapshot directory that gets created and later restore it as a data directory, as needed.
createSnapshot :: (MonadTypesense m) => CreateSnapshot -> m OperationsResponse
createSnapshot CreateSnapshot{..} = do
  c <- askTypesenseClient
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withQueryString [("snapshot_path", Just $ T.encodeUtf8 $ toQueryParam snapshotPath)] $
    withPathAndMethod methodPost ["operations", "snapshot"] c

-- | Triggers a follower node to initiate the raft voting process, which triggers leader re-election.
--
-- The follower node that you run this operation against will become the new leader, once this command succeeds.
reElectLeader :: (MonadTypesense m) => m OperationsResponse
reElectLeader = do
  c <- askTypesenseClient
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodPost ["operations", "vote"] c

-- | Get current RAM, CPU, Disk & Network usage metrics.
clusterMetrics :: (MonadTypesense m) => m ClusterMetrics 
clusterMetrics = do
  c <- askTypesenseClient
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodGet ["metrics.json"] c

-- | Get stats about API endpoints.
--
-- This endpoint returns average requests per second and latencies for all requests in the last 10 seconds.
apiStats :: (MonadTypesense m) => m APIStats
apiStats = do
  c <- askTypesenseClient
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodGet ["stats.json"] c

-- | Get health information about a Typesense node.
health :: (MonadTypesense m) => m Health
health = do
  c <- askTypesenseClient
  fmap getResponseBody $
    httpJSON $
    typesenseRequest $
    withPathAndMethod methodGet ["health"] c
