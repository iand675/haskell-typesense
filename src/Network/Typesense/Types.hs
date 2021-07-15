{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleInstances #-}
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
  toJSON CollectionSchemaField{..} = object (alwaysFields <> maybeFields)
    where
      alwaysFields =
        [ "name" .= name
        , "type" .= type_
        ]
      maybeFields = catMaybes
        [ ("facet" .=) <$> facet
        , ("optional" .=) <$> optional
        , ("index" .=) <$> index
        ]

instance FromJSON CollectionSchemaField where
  parseJSON = withObject "CollectionSchemaField" $ \o ->
    CollectionSchemaField <$>
      o .: "name" <*>
      o .: "type" <*>
      o .:? "facet" <*>
      o .:? "optional" <*>
      o .:? "index"

data CollectionSchema = CollectionSchema
  { name :: Text
  , fields :: [] CollectionSchemaField
  , defaultSortingField :: Maybe Text
  } deriving (Show)

instance ToJSON CollectionSchema where
  toJSON CollectionSchema{..} = object (alwaysFields <> maybeFields)
    where
      alwaysFields =
        [ "name" .= name
        , "fields" .= fields
        ]
      maybeFields = catMaybes
        [ ("default_sorting_field" .=) <$> defaultSortingField
        ]

data Collection = Collection
  { name :: Text
  , fields :: [] CollectionSchemaField
  , numDocuments :: Word64
  , defaultSortingField :: Maybe Text
  } deriving (Show)

instance FromJSON Collection where
  parseJSON = withObject "Collection" $ \o ->
    Collection <$>
      o .: "name" <*>
      o .: "fields" <*>
      o .: "num_documents" <*>
      o .:? "default_sorting_field"

newtype CollectionName a = CollectionName
  { collectionName :: Text
  } deriving (ToHttpApiData)

newtype DocumentId a = DocumentId Text
  deriving (Show, Eq, ToJSON, FromJSON)

-- TODO... 
data Document a = Document
  { id :: DocumentId a
  , document :: a
  } deriving (Show)

instance FromJSON a => FromJSON (Document a) where
  parseJSON = withObject "Document" $ \o -> do
    oid <- o .: "id"
    wrappedValue <- parseJSON (Object o)
    pure $ Document oid wrappedValue

newtype Client = Client
  { typesenseRequest :: Request
  }

class HasTypesenseClient a where
  getTypesenseClient :: a -> Client

instance HasTypesenseClient Client where
  getTypesenseClient = Prelude.id

class MonadIO m => MonadTypesense m where
  askTypesenseClient :: m Client

instance (HasTypesenseClient c, MonadIO m) => MonadTypesense (ReaderT c m) where
  askTypesenseClient = asks getTypesenseClient

newtype ApiKey = ApiKey Text
  deriving (ToHttpApiData)

data QueryBy = QueryBy
  { by :: Text
  , weight :: Maybe Int
  }

data SearchQuery = SearchQuery
  { q :: Text
  , queryBy :: NonEmpty Text 
  -- ^ Fields to query against
  , queryByWeights :: [Int]
  -- ^ Weights to allocate each field.
  , prefix :: [Bool]
  -- TODO
  -- , filterBy
  -- TODO
  -- , sortBy
  -- TODO
  -- , facetBy
  -- TODO
  -- , maxFacetValues
  -- TODO
  -- , facetQuery
  -- TODO
  -- , prioritizeExactMatch
  , page :: Maybe Int
  , perPage :: Maybe Int
  , groupBy :: [Text]
  , groupLimit :: Maybe Int
  , includeFields :: [Text]
  , excludeFields :: [Text]
  , highlightFields :: [Text]
  -- TODO more stuff
  }

query :: Text -> NonEmpty Text -> SearchQuery
query q queryBy = SearchQuery
  { q = q
  , queryBy = queryBy
  , queryByWeights = []
  , prefix = []
  , page = Nothing
  , perPage = Nothing
  , groupBy = []
  , groupLimit = Nothing
  , includeFields = []
  , excludeFields = []
  , highlightFields = []
  }

searchQueryToParams :: SearchQuery -> [(ByteString, Maybe ByteString)]
searchQueryToParams SearchQuery{..} = map (Data.Bifunctor.second Just) $ catMaybes
  [ Just ("q", T.encodeUtf8 $ q)
  , Just ("query_by", T.encodeUtf8 $ T.intercalate "," $ NE.toList queryBy)
  , (\num -> ("per_page", T.encodeUtf8 $ toQueryParam num)) <$> perPage
  , case queryByWeights of
      [] -> Nothing
      _ -> Just ("query_by_weights", T.encodeUtf8 $ T.intercalate "," $ fmap toQueryParam queryByWeights)
  ]
    -- queryByWeightsParam = case mapMaybe $ NonEmpty.toList $ weight queryBy of
    --  [] -> 

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

data SearchResult a = SearchResult
  { hits :: [Hit a]
  , searchTimeMs :: Int
  } deriving (Show)

instance FromJSON a => FromJSON (SearchResult a) where
  parseJSON = withObject "SearchResult" $ \o ->
    SearchResult <$>
      o .: "hits" <*>
      o .: "search_time_ms"
