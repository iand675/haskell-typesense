{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.Typesense.Types.Internal where
import Data.Aeson
import Network.Typesense.Types

newtype NewAlias a = NewAlias
  { collectionName :: CollectionName a
  }

instance ToJSON (NewAlias a) where
  toJSON NewAlias{..} = object [ "collection_name" .= collectionName ]