{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad.Reader
import Network.Typesense
import qualified Data.ByteString.Lazy as L
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text
import GHC.Generics
import Data.Aeson
import Control.Monad.Catch

data AdminOmniSearchUser = AdminOmniSearchUser
  { email :: Text
  , fullName :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON AdminOmniSearchUser

data AdminOmniSearchElement = AdminOmniSearchElement
  { -- id :: Text
    callsign :: Text
  , organizationName :: Text
  , primaryUserId :: Text
  , organizationStatus :: Text
  , adminOmniSearchUsers :: [AdminOmniSearchUser]
  , accountNumbers :: [Text]
  }
  deriving stock (Show, Generic)

instance FromJSON AdminOmniSearchElement

newtype Results = Results
  { results :: [AdminOmniSearchElement]
  } deriving stock (Show, Generic)

instance FromJSON Results

data ReformulatedSearchElement = ReformulatedSearchElement
  { -- id :: Text
    callsign :: Text
  , organizationName :: Text
  , primaryUserId :: Text
  , organizationStatus :: Text
  , userFullNames :: [Text]
  , userEmails :: [Text]
  , accountNumbers :: [Text]
  } deriving (Show, Generic)

instance ToJSON ReformulatedSearchElement
instance FromJSON ReformulatedSearchElement

reformulateSearchElement :: AdminOmniSearchElement -> ReformulatedSearchElement
reformulateSearchElement AdminOmniSearchElement{..} = ReformulatedSearchElement
  { -- id = id
    callsign = callsign
  , organizationName = organizationName
  , primaryUserId = primaryUserId
  , organizationStatus = organizationStatus
  , userFullNames = fmap fullName adminOmniSearchUsers
  , userEmails = fmap email adminOmniSearchUsers
  , accountNumbers = accountNumbers
  }

recreateCollection :: MonadTypesense m => m Collection
recreateCollection = do
  dropCollection $ CollectionName "adminOrgs"
  createCollection $ CollectionSchema
    { name = "adminOrgs"
    , fields =
        [ CollectionSchemaField
            { name = "callsign"
            , type_ = ScalarField StringField
            , facet = Nothing
            , optional = Nothing
            , index = Nothing
            }
        , CollectionSchemaField
            { name = "organizationName"
            , type_ = ScalarField StringField
            , facet = Nothing
            , optional = Nothing
            , index = Nothing
            }
        , CollectionSchemaField
            { name = "primaryUserId"
            , type_ = ScalarField StringField
            , facet = Nothing
            , optional = Nothing
            , index = Nothing
            }
        , CollectionSchemaField
            { name = "organizationStatus"
            , type_ = ScalarField StringField
            , facet = Nothing
            , optional = Nothing
            , index = Nothing
            }
        , CollectionSchemaField
            { name = "userEmails"
            , type_ = ArrayField StringField
            , facet = Nothing
            , optional = Nothing
            , index = Nothing
            }
        , CollectionSchemaField
            { name = "userFullNames"
            , type_ = ArrayField StringField
            , facet = Nothing
            , optional = Nothing
            , index = Nothing
            }
        , CollectionSchemaField
            { name = "accountNumbers"
            , type_ = ArrayField StringField
            , facet = Nothing
            , optional = Nothing
            , index = Nothing
            }
        ]
    , defaultSortingField = Nothing
    }

indexDocs :: MonadTypesense m => m ()
indexDocs = do
  res <- eitherDecode <$> liftIO (L.readFile "corpus.json")
  case res of
    Left err -> liftIO $ print err
    Right parsedStuff -> do
      let reformulated = reformulateSearchElement <$> results parsedStuff
      mapM_ (upsertDocument (CollectionName "adminOrgs")) reformulated

runTypesense :: (MonadIO m, MonadThrow m) => ReaderT Client m a -> m a
runTypesense m = do
  let k = ApiKey "Hu52dwsas2AdxdE"
  c <- mkClient "http://localhost:8108" k
  runReaderT m c

main :: IO ()
main = runTypesense $ do
  recreateCollection 
  indexDocs

