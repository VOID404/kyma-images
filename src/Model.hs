{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Model where

import           Data.Aeson
import           Data.Aeson.Types (prependFailure, typeMismatch)
import           Data.Text
import           Data.Time        (UTCTime)

data PrResponse
  = PrResponse
  { url            :: Text
  , title          :: Text
  , mergeCommitSha :: Text
  , merged         :: Bool
  , mergeDate      :: UTCTime
  } deriving (Show)

instance FromJSON PrResponse where
  parseJSON (Object v) = do
    url <- v .: "url"
    title <- v .: "title"
    mergeCommitSha <- v .: "merge_commit_sha"
    merged <- v .: "merged"
    mergeDate <- v .: "merged_at"
    pure $ PrResponse{..}
  parseJSON invalid = do
    prependFailure "parsing PrResponse failed, "
      (typeMismatch "Object" invalid)
