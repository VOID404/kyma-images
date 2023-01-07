{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Lib where

import           Control.Concurrent.Async (mapConcurrently_)
import           Data.Aeson               (FromJSON (..), ToJSON)
import           Data.Functor
import           Data.Text                (Text, append, pack, unpack)
import           Data.Time                (defaultTimeLocale, formatTime)
import           GitHub.REST              (GHEndpoint (GHEndpoint, endpoint, endpointVals, ghData, method),
                                           GitHubSettings (GitHubSettings, apiVersion, token, userAgent),
                                           KeyValue ((:=)),
                                           MonadGitHubREST (queryGitHub),
                                           StdMethod (GET), Token, runGitHubT)
import           Model
import           Options.Applicative
import           System.FilePath.Find
import           Text.Regex.TDFA          ((=~))


newtype CmdOpts = CmdOpts
  { dir     :: FilePath
  } deriving (Show)

run :: CmdOpts -> IO ()
run CmdOpts{dir = dir} = do
  files <- findYamls dir
  mapConcurrently_ processFile files

findYamls :: FilePath -> IO [FilePath]
findYamls = find always (fileName ==? "values.yaml")

cmdOpts :: Parser CmdOpts
cmdOpts = CmdOpts
          <$> argument str
              ( metavar "<folder>"
             <> help "Folder containing kyma sources" )

processFile :: FilePath -> IO ()
processFile path = do
  let auth = ghAuth Nothing

  content <- readFile path

  newContent <- processContent ((<&> pack . naiveTag) . getMerge auth) $ pack content

  writeFile path $ unpack newContent

-- desired format: v20221227-2dddc00f
naiveTag :: PrResponse -> String
naiveTag pr = "v" ++ timeStr ++ "-" ++ take 8 commit
  where
    time = mergeDate pr
    timeStr = formatTime defaultTimeLocale "%Y%m%d" time
    commit = unpack $ mergeCommitSha pr

processContent :: (Int -> IO Text) ->  Text -> IO Text
processContent f file = case (file =~ ("PR-([[:digit:]]+)" :: Text) :: (Text, Text, Text, [Text])) of
                        (pre, "", "", [])   -> pure pre
                        (pre, _, post, [p]) -> let old = read $ unpack p
                                                   new = f old
                                                in append
                                                   <$> new
                                                   <*> processContent f post
                                                   <&> append pre
                        v                   -> error $ "the fuck is this ;_; " ++ show v

kymaOrg :: Text
kymaOrg = "kyma-project"
kymaRepo :: Text
kymaRepo = "kyma"

getMergeGH :: (MonadGitHubREST m, FromJSON a) => Int -> m a
getMergeGH pr = queryGitHub GHEndpoint
  { method = GET -- /repos/:owner/:repo/git/refs/:ref
  , endpoint = "/repos/:owner/:repo/pulls/:pr"
  , endpointVals =
    [ "owner" := kymaOrg
    , "repo" := kymaRepo
    , "pr" := pr
    ]
  , ghData = []
  }

getMerge :: GitHubSettings -> Int -> IO PrResponse
getMerge auth pr = runGitHubT auth $ getMergeGH pr

ghAuth :: Maybe Token -> GitHubSettings
ghAuth tk = GitHubSettings
             { token = tk
                -- ^ An authentication token to use, if any.
              , userAgent = "void/kyma-images"
                -- ^ GitHub requires this to be set to a User Agent specific to your
                -- application: https://developer.github.com/v3/#user-agent-required
              , apiVersion = "v3"
                -- ^ Specifies the API version to query: https://developer.github.com/v3/media/
              }
