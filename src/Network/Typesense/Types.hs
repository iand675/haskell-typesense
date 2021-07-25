{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Typesense.Types where
import Control.Monad.Reader.Class ( MonadReader, asks )
import Control.Monad.Trans ( MonadIO )
import Data.Aeson
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import Data.Maybe
import Network.HTTP.Client (Request)
import Web.HttpApiData
import Control.Monad.Reader
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Network.HTTP.Simple (Query)
import Data.ByteString.Char8 (ByteString)
import qualified Data.Bifunctor
import Data.String (IsString)
import System.Posix.Types
import qualified Data.HashMap.Strict as HM
import Control.Monad.Catch
import GHC.Generics
import Data.IORef
import Data.Vector (Vector)

-- | JSON Maybe utility
(.=?) :: ToJSON a => Text -> Maybe a -> [(Text, Value)] -> [(Text, Value)]
k .=? mv = \xs -> case mv of
  Nothing -> xs
  Just v -> (k .= v) : xs

infixl .=?

newtype Id a = Id Text
  deriving (Show, Eq, Ord, ToJSON, ToJSONKey, FromJSON, FromJSONKey, Functor, Generic, ToHttpApiData, FromHttpApiData)

newtype IdOnlyResponse a = IdOnlyResponse { id :: Id a }
  deriving (Show, Eq, Ord, Functor, Generic)

instance FromJSON (IdOnlyResponse a) where
  parseJSON = withObject "IdOnlyResponse" $ \o ->
    IdOnlyResponse <$> o .: "id"

data Entity a = Entity
  { id :: Id a
  , value :: a
  }

instance ToJSON a => ToJSON (Entity a) where
  toJSON Entity{..} = case toJSON value of
    Object o -> Object (HM.insert "id" (toJSON id) o)
    other -> error (show other ++ " must be an object for Entity instance to work")

instance FromJSON a => FromJSON (Entity a) where
  parseJSON = withObject "Entity" $ \o ->
    Entity <$>
      o .: "id" <*>
      parseJSON (Object o)

data ScalarType
  = StringField
  | Int32Field
  | Int64Field
  | FloatField
  | BoolField
  | GeopointField
  deriving (Show)

data FieldType
  = ScalarField ScalarType
  | ArrayField ScalarType
  | AutoField
  deriving (Show)

scalarFieldType :: ScalarType -> Text
scalarFieldType = \case
  StringField -> "string"
  Int32Field -> "int32"
  Int64Field -> "int64"
  FloatField -> "float"
  BoolField -> "bool"
  GeopointField -> "geopoint"

instance ToJSON FieldType where
  toJSON = String . \case
    (ScalarField t) -> scalarFieldType t
    (ArrayField scalar) -> scalarFieldType scalar <> "[]"
    AutoField -> "auto"

instance FromJSON FieldType where
  parseJSON = withText "FieldType" $ \case
    "string" -> pure $ ScalarField StringField
    "int32"-> pure $ ScalarField Int32Field
    "int64"-> pure $ ScalarField Int64Field
    "float"-> pure $ ScalarField FloatField
    "bool"-> pure $ ScalarField BoolField
    "geopoint"-> pure $ ScalarField GeopointField
    "string[]"-> pure $ ArrayField StringField
    "int32[]"-> pure $ ArrayField Int32Field
    "int64[]"-> pure $ ArrayField Int64Field
    "float[]"-> pure $ ArrayField FloatField
    "bool[]"-> pure $ ArrayField StringField
    "geopoint[]" -> pure $ ArrayField GeopointField
    "auto" -> pure AutoField
    str -> fail ("Cannot decode type " <> show str)

data CollectionSchemaField = CollectionSchemaField
  { name :: Text
  , type_ :: FieldType
  , facet :: Maybe Bool
  , optional :: Maybe Bool
  , index :: Maybe Bool
  } deriving (Show)

instance ToJSON CollectionSchemaField where
  toJSON CollectionSchemaField{..} = object 
    ( "facet" .=? facet $
      "optional" .=? optional $
      "index" .=? index $
      alwaysFields
    )
    where
      alwaysFields =
        [ "name" .= name
        , "type" .= type_
        ]

instance FromJSON CollectionSchemaField where
  parseJSON = withObject "CollectionSchemaField" $ \o ->
    CollectionSchemaField <$>
      o .: "name" <*>
      o .: "type" <*>
      o .:? "facet" <*>
      o .:? "optional" <*>
      o .:? "index"

data CollectionSchema a = CollectionSchema
  { name :: CollectionName a
  , fields :: [] CollectionSchemaField
  , defaultSortingField :: Maybe Text
  } deriving (Show)

instance ToJSON (CollectionSchema a) where
  toJSON CollectionSchema{name=CollectionName{collectionName}, ..} = object 
    ("default_sorting_field" .=? defaultSortingField $ alwaysFields)
    where
      alwaysFields =
        [ "name" .= collectionName
        , "fields" .= fields
        ]

data Collection a = Collection
  { name :: CollectionName a
  , fields :: [] CollectionSchemaField
  , numDocuments :: Word64
  , defaultSortingField :: Maybe Text
  } deriving (Show, Functor)

instance FromJSON (Collection a) where
  parseJSON = withObject "Collection" $ \o ->
    Collection <$>
      o .: "name" <*>
      o .: "fields" <*>
      o .: "num_documents" <*>
      o .:? "default_sorting_field"

newtype CollectionName a = CollectionName
  { collectionName :: Text
  } deriving (Show, Eq, Ord, ToHttpApiData, IsString, ToJSON, ToJSONKey, FromJSON, FromJSONKey, Functor)

newtype DocumentId a = DocumentId Text
  deriving (Show, Eq, ToJSON, ToJSONKey, FromJSON, FromJSONKey, Functor)

-- TODO... 
data Document a = Document
  { id :: DocumentId a
  , document :: a
  } deriving (Show, Functor)

instance FromJSON a => FromJSON (Document a) where
  parseJSON = withObject "Document" $ \o -> do
    oid <- o .: "id"
    wrappedValue <- parseJSON (Object o)
    pure $ Document oid wrappedValue

data Client = Client
  { typesenseHosts :: Vector Request
  , lastUsedHostIndex :: IORef Int
  , connectionTimeoutSeconds :: !Int
  , apiKey :: APIKey
  }

class HasTypesenseClient a where
  getTypesenseClient :: a -> Client

instance HasTypesenseClient Client where
  getTypesenseClient = Prelude.id

class (MonadIO m, MonadCatch m) => MonadTypesense m where
  askTypesenseClient :: m Client

instance (HasTypesenseClient c, MonadIO m, MonadCatch m) => MonadTypesense (ReaderT c m) where
  askTypesenseClient = asks getTypesenseClient

newtype ApiKey = ApiKey Text
  deriving (ToHttpApiData)

data QueryBy = QueryBy
  { by :: Text
  , weight :: Maybe Int
  }

data SortOrder = Ascending | Descending

-- TODO figure out the scoped API Key serialization
data ParamValue
  = StringParam !Text
  | BoolParam !Bool
  | IntParam !Int
  deriving (Show)

instance ToHttpApiData ParamValue where
  toQueryParam = \case
    StringParam p -> toQueryParam p
    BoolParam p -> toQueryParam p
    IntParam p -> toQueryParam p

instance ToJSON ParamValue where
  toJSON  = \case
    StringParam p -> toJSON p
    BoolParam p -> toJSON p
    IntParam p -> toJSON p


newtype QueryParams = QueryParams
  { queryParams :: HM.HashMap Text ParamValue
  } deriving (Show, Semigroup, Monoid, ToJSON)

data SearchQuery = SearchQuery
  { q :: Text
  , queryBy :: NonEmpty Text
  , otherQueryParams :: QueryParams
  }

data SearchQueryParameter a = SearchQueryParameter
  { paramName :: Text
  , queryParamSerializer :: a -> ParamValue
  }

p :: Text -> (a -> ParamValue) -> SearchQueryParameter a
p = SearchQueryParameter

(.->) :: SearchQueryParameter a -> a -> SearchQuery -> SearchQuery
paramName .-> val = \q -> q { otherQueryParams = addParam paramName val (otherQueryParams q) }

infixl .->

addParam :: SearchQueryParameter a -> a -> QueryParams -> QueryParams
addParam SearchQueryParameter{..} p (QueryParams m) = QueryParams $ HM.insert paramName (queryParamSerializer p) m

queryByWeights :: SearchQueryParameter [Int]
queryByWeights = p "query_by_weights" (StringParam . T.intercalate "," . map toQueryParam)

prefix :: SearchQueryParameter [Bool]
prefix = p "prefix" (StringParam . T.intercalate "," . map toQueryParam)

filterBy :: SearchQueryParameter Text -- TODO need to support an ADT version
filterBy = p "filter_by" StringParam

sortBy :: SearchQueryParameter [(Text, SortOrder)]
sortBy = p "sort_by" (StringParam . T.intercalate "," . map (\(k, v) -> T.concat [k, ":", orderVal v]))
  where
    orderVal Ascending = "asc"
    orderVal Descending = "desc"

facetBy :: SearchQueryParameter [Text]
facetBy = p "facet_by" (StringParam . T.intercalate "," . map toQueryParam)

maxFacetValues :: SearchQueryParameter Int
maxFacetValues = p "max_facet_values" IntParam

facetQuery :: SearchQueryParameter (Text, Text {- TODO could potentially support something nicer than text here-})
facetQuery = p "facet_query" (StringParam . (\(k, v) -> T.concat [k, ":", v]))

prioritizeExactMatch :: SearchQueryParameter Bool
prioritizeExactMatch = p "prioritize_exact_match" BoolParam

page :: SearchQueryParameter Int
page = p "page" IntParam

perPage :: SearchQueryParameter Int
perPage = p "per_page" IntParam

groupBy :: SearchQueryParameter [Text]
groupBy = p "group_by" (StringParam . T.intercalate ",")

groupLimit :: SearchQueryParameter Int
groupLimit = p "group_limit" IntParam

includeFields :: SearchQueryParameter [Text]
includeFields = p "include_fields" (StringParam . T.intercalate ",")

excludeFields :: SearchQueryParameter [Text]
excludeFields = p "exclude_fields" (StringParam . T.intercalate ",")

highlightFields :: SearchQueryParameter [Text]
highlightFields = p "highlight_fields" (StringParam . T.intercalate ",")

highlightFullFields :: SearchQueryParameter [Text]
highlightFullFields = p "highlight_full_fields" (StringParam . T.intercalate ",")

highlightAffixNumTokens :: SearchQueryParameter Int
highlightAffixNumTokens = p "highlight_affix_num_tokens" IntParam

highlightStartTag :: SearchQueryParameter Text
highlightStartTag = p "highlight_start_tag" StringParam

highlightEndTag :: SearchQueryParameter Text
highlightEndTag = p "highlight_end_tag" StringParam

snippetThreshold :: SearchQueryParameter Int
snippetThreshold = p "snippet_threshold" IntParam

numTypos :: SearchQueryParameter Int
numTypos = p "num_typos" IntParam

typoTokensThreshold :: SearchQueryParameter Int
typoTokensThreshold = p "typo_tokens_threshold" IntParam

dropTokensThreshold :: SearchQueryParameter Int
dropTokensThreshold = p "drop_tokens_threshold" IntParam

pinnedHits :: SearchQueryParameter [(Text, Text)]
pinnedHits = p "pinned_hits" (StringParam . T.concat . map (\(k, v) -> T.concat [k, ":", v]))

hiddenHits :: SearchQueryParameter [Text]
hiddenHits = p "hidden_hits" (StringParam . T.intercalate ",")

enableOverrides :: SearchQueryParameter Bool
enableOverrides = p "enable_overrides" BoolParam

preSegmentedQuery :: SearchQueryParameter Bool
preSegmentedQuery = p "pre_segmented_query" BoolParam

limitHits :: SearchQueryParameter Int
limitHits = p "limit_hits" IntParam

query :: Text -> NonEmpty Text -> SearchQuery
query q queryBy = SearchQuery
  { q = q
  , queryBy = queryBy
  , otherQueryParams = mempty
  }

searchQueryObject :: SearchQuery -> Object
searchQueryObject SearchQuery{..} =
  HM.insert "q" (toJSON q) $
  HM.insert "query_by" (toJSON q) $
  HM.map toJSON . queryParams $
  otherQueryParams

searchQueryToParams :: SearchQuery -> [(ByteString, Maybe ByteString)]
searchQueryToParams SearchQuery{..} =
  [ ("q", Just $ T.encodeUtf8 q)
  , ("query_by", Just $ T.encodeUtf8 $ T.intercalate "," $ NE.toList queryBy)
  ] <> extraParams
  where
    extraParams = map (\(k, v) -> (T.encodeUtf8 k, Just $ T.encodeUtf8 $ toQueryParam v)) $ HM.toList $ queryParams otherQueryParams
    -- queryByWeightsParam = case mapMaybe $ NonEmpty.toList $ weight queryBy of
    --  [] -> 

instance ToJSON SearchQuery where
  toJSON = toJSON . searchQueryObject

data Highlight = Highlight
  { field :: Text
  , snippet :: Maybe Text
  , matchedTokens :: [Value]
  } deriving (Show)

instance FromJSON Highlight where
  parseJSON = withObject "Highlight" $ \o ->
    Highlight <$>
      o .: "field" <*>
      o .:? "snippet" <*>
      o .: "matched_tokens"

data Hit a = Hit
  { document :: Document a
  , highlights :: [Highlight]
  , textMatch :: Word64
  } deriving (Show)

instance FromJSON a => FromJSON (Hit a) where
  parseJSON = withObject "Hit" $ \o ->
    Hit <$>
      o .: "document" <*>
      o .: "highlights" <*>
      o .: "text_match"

data SearchResultShape 
  = Regular
  | Grouped

data SearchResult (shape :: SearchResultShape) a = SearchResult
  { hits :: [Hit a]
  , searchTimeMs :: Int
  } deriving (Show)

instance FromJSON a => FromJSON (SearchResult a) where
  parseJSON = withObject "SearchResult" $ \o ->
    SearchResult <$>
      o .: "hits" <*>
      o .: "search_time_ms"

data Alias a = Alias
  { name :: CollectionName a
  , collectionName :: CollectionName a
  }

instance FromJSON (Alias a) where
  parseJSON = withObject "Alias" $ \o ->
    Alias <$>
      o .: "name" <*>
      o .: "collection_name"

newtype Aliases = Aliases
  { aliases :: [Alias Object]
  }

instance FromJSON Aliases where
  parseJSON = withObject "Aliases" $ \o ->
    Aliases <$>
      o .: "aliases"


data APIKeyAction
  = DocumentsSearch
  | DocumentsGet
  | CollectionsDelete
  | CollectionsCreate
  | CollectionsAllOperations
  | AllOperations

instance ToJSON APIKeyAction where
  toJSON = String . \case
    DocumentsSearch -> "documents:search"
    DocumentsGet -> "documents:get"
    CollectionsDelete -> "collections:delete"
    CollectionsCreate -> "collections:create"
    CollectionsAllOperations -> "collections:*"
    AllOperations -> "*"

instance FromJSON APIKeyAction where
  parseJSON = withText "APIKeyAction" $ \case
    "documents:search" -> pure DocumentsSearch
    "documents:get" -> pure DocumentsGet
    "collections:delete" -> pure CollectionsDelete
    "collections:create" -> pure CollectionsCreate
    "collections:*" -> pure CollectionsAllOperations
    "*" -> pure AllOperations
    str -> fail (show str <> " is not an action that the Haskell typesense library understands. Please file an issue or pull request.")

newtype APIKey = APIKey { apiKey :: ByteString }
  deriving (Show, Eq, Ord)

instance FromJSON APIKey where
  parseJSON = withText "APIKey" (pure . APIKey . T.encodeUtf8)

data KeyInfo = KeyInfo
  { actions :: [APIKeyAction]
  , collections :: [CollectionName Object]
  , description :: Maybe Text
  }

keyInfoToObject :: KeyInfo -> Object
keyInfoToObject KeyInfo{..} = HM.fromList
  [ "actions" .= actions
  , "collections" .= collections
  , "description" .= description
  ]

instance FromJSON KeyInfo where
  parseJSON = withObject "KeyInfo" $ \o ->
    KeyInfo <$>
      o .: "actions" <*>
      o .: "collections" <*>
      o .:? "description"

data NewAPIKey = NewAPIKey
  { value :: Maybe Text
  , expiresAt :: Maybe EpochTime
  , keyInfo :: KeyInfo
  }

instance ToJSON NewAPIKey where
  toJSON NewAPIKey{..} = Object
    (HM.fromList (catMaybes
      [ ("value" .=) <$> value
      , ("expires_at" .=) <$> expiresAt
      ]) <>
      keyInfoToObject keyInfo
    )


data CreatedAPIKey = CreatedAPIKey
  { id :: Text
  , value :: APIKey
  , keyInfo :: KeyInfo
  }

instance FromJSON CreatedAPIKey where
  parseJSON = withObject "CreatedAPIKey" $ \o ->
    CreatedAPIKey <$>
      o .: "id" <*>
      o .: "value" <*>
      parseJSON (Object o)


data ExistingAPIKey = ExistingAPIKey
  { id :: Text
  , valuePrefix :: Text
  , keyInfo :: KeyInfo
  }

instance FromJSON ExistingAPIKey where
  parseJSON = withObject "ExistingAPIKey" $ \o ->
    ExistingAPIKey <$>
      o .: "id" <*>
      o .: "value_prefix" <*>
      parseJSON (Object o)

newtype DeletedAPIKey = DeletedAPIKey
  { id :: Text
  }

instance FromJSON DeletedAPIKey where
  parseJSON = withObject "DeletedAPIKey" $ \o ->
    DeletedAPIKey <$>
      o .: "id"

data SearchQueryForAPIKey =
  SearchQueryForAPIKey
    { searchQuery :: SearchQuery
    , expiresAt :: Maybe EpochTime
    }

instance ToJSON SearchQueryForAPIKey where
  toJSON SearchQueryForAPIKey{..} = case expiresAt of
    Nothing -> toJSON searchQuery
    Just t -> case toJSON searchQuery of
      Object o -> Object (HM.insert "expires_at" (toJSON t) o)
      other -> error ("SearchQuery must serialize to a JSON object: got " <> show other)

newtype ListAPIKeys = ListAPIKeys
  { keys :: [ExistingAPIKey]
  }

instance FromJSON ListAPIKeys where
  parseJSON = withObject "ListAPIKeys" $ \o -> 
    ListAPIKeys <$> 
      o .: "keys"




data Synonym = Synonym
  { synonyms :: [Text]
  , root :: Maybe Text
  }

instance ToJSON Synonym where
  toJSON Synonym{..} = object
    ( "root" .=? root $
      ["synonyms" .= synonyms]
    )

instance FromJSON Synonym where
  parseJSON = withObject "Synonym" $ \o ->
    Synonym <$>
      o .: "synonyms" <*>
      o .:? "root"

newtype SynonymsList = SynonymsList
  { synonyms :: [Entity Synonym]
  }

instance FromJSON SynonymsList where
  parseJSON = withObject "SynonymsList" $ \o ->
    SynonymsList <$>
      o .: "synonyms"


newtype CreateSnapshot = CreateSnapshot
  { snapshotPath :: FilePath
  }

newtype SlowlogConfig = SlowlogConfig
  { logSlowRequestsTimeMs :: Int64
  }

newtype OperationsResponse = OperationsResponse
  { success :: Bool
  }

instance FromJSON OperationsResponse where
  parseJSON = withObject "OperationsResponse" $ \o ->
    OperationsResponse <$> o .: "success"

newtype ClusterMetrics = ClusterMetrics
  { metrics :: Object
  } deriving (Show)

instance FromJSON ClusterMetrics where
  parseJSON = withObject "ClusterMetrics" $ \o ->
    ClusterMetrics <$> o .: "metrics"

data APIStats = APIStats
  { latencyMs :: HM.HashMap Text Double
  , requestsPerSecond :: HM.HashMap Text Double
  }

instance FromJSON APIStats where
  parseJSON = withObject "APIStats" $ \o ->
    APIStats <$>
      o .: "latency_ms" <*>
      o .: "requests_per_second"

newtype Health = Health
  { status :: Text
  }

instance FromJSON Health where
  parseJSON = withObject "Health" $ \o ->
    Health <$> o .: "status"
