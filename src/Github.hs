{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Github where

import           Data.Aeson  (FromJSON (..))
import           Data.Text   (Text)
import           GitHub.REST (GHEndpoint (GHEndpoint, endpoint, endpointVals, ghData, method),
                              GitHubSettings (GitHubSettings, apiVersion, token, userAgent),
                              KeyValue ((:=)), MonadGitHubREST (queryGitHub),
                              StdMethod (GET), Token, runGitHubT)
import           Model

-- |Hardcoded project organization name
kymaOrg :: Text
kymaOrg = "kyma-project"

-- |Hardcoded project repo name
kymaRepo :: Text
kymaRepo = "kyma"

-- |`GHEndpoint` fetching a
-- [GitHub PR](https://docs.github.com/en/rest/pulls/pulls?apiVersion=2022-11-28#get-a-pull-request)
-- based on it's number.
getMerge' :: (MonadGitHubREST m, FromJSON a) => Int -> m a
getMerge' pr = queryGitHub GHEndpoint
  { method = GET -- /repos/:owner/:repo/git/refs/:ref
  , endpoint = "/repos/:owner/:repo/pulls/:pr"
  , endpointVals =
    [ "owner" := kymaOrg
    , "repo" := kymaRepo
    , "pr" := pr
    ]
  , ghData = []
  }

-- |Simplified version of 'getMerge'',
-- only use this if you don't care for chaining GitHub requests.
getMerge :: GitHubSettings -> Int -> IO PrResponse
getMerge auth pr = runGitHubT auth $ getMerge' pr

githubAuth :: Maybe Token -> GitHubSettings
githubAuth tk = GitHubSettings
             { token = tk
                -- ^ An authentication token to use, if any.
              , userAgent = "void/kyma-images"
                -- ^ GitHub requires this to be set to a User Agent specific to your
                -- application: https://developer.github.com/v3/#user-agent-required
              , apiVersion = "v3"
                -- ^ Specifies the API version to query: https://developer.github.com/v3/media/
              }
