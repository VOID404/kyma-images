{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Lib where

import           Control.Concurrent.Async (mapConcurrently_)
import           Data.ByteString          (ByteString)
import           Data.Functor
import           Data.Text                (Text, append, pack, unpack)
import           Data.Time                (defaultTimeLocale, formatTime)
import           Github                   (getMerge, githubAuth)
import           GitHub.REST              (Token (AccessToken, BearerToken))
import           Model
import           Options.Applicative
import           System.FilePath.Find
import           Text.Regex.TDFA          ((=~))


-- | Docker image tag
type Tag = Text

data CmdOpts = CmdOpts
                { dir         :: FilePath
                , accessToken :: Maybe ByteString
                , bearerToken :: Maybe ByteString
                } deriving (Show)

-- |Command line arguments parser, used for `run`
cmdOpts :: Parser CmdOpts
cmdOpts = CmdOpts
          <$> argument str
              ( metavar "<folder>"
             <> help "Folder containing kyma sources" )
          <*> option auto
              ( long "access-token"
             <> short 'a'
             <> metavar "<token>"
             <> value Nothing
             <> help "GitHub access token. https://developer.github.com/v3/#authentication")
          <*> option auto
              ( long "bearer-token"
             <> short 'b'
             <> metavar "<token>"
             <> value Nothing
             <> help "GitHub bearer token. https://developer.github.com/apps/building-github-apps/authenticating-with-github-apps/#authenticating-as-a-github-app")

run :: CmdOpts -> IO ()
run CmdOpts{ dir = dir
           , accessToken = accessToken
           , bearerToken = bearerToken
           } = do

  files <- findYamls dir
  let token = AccessToken <$> accessToken
          <|> BearerToken <$> bearerToken

  mapConcurrently_ (processFile token) files

-- |Finds all @values.yaml@ files in a given path and returns them in a list
findYamls :: FilePath -> IO [FilePath]
findYamls = find always (fileName ==? "values.yaml")


-- |Replace all PR images in file specified by `FilePath`
processFile :: Maybe Token -> FilePath -> IO ()
processFile token path = do
  let auth = githubAuth token
  content <- readFile path
  newContent <- replacePrImages ((<&> naiveTag) . getMerge auth) $ pack content

  writeFile path $ unpack newContent

-- |Generates image tag based on `PrResponse`.
-- Desired format: @"v" ++ \"YYYYMMDD\" ++ "-" ++ take 8 commitSha@
naiveTag :: PrResponse -> Tag
naiveTag pr = pack $ "v" ++ timeStr ++ "-" ++ take 8 commit
  where
    time = mergeDate pr
    timeStr = formatTime defaultTimeLocale "%Y%m%d" time
    commit = unpack $ mergeCommitSha pr

-- |Replaces all PR images in given `Text`
-- using a function that turns commit # into a `Tag`.
-- Uses regex, so beware.
replacePrImages :: (Int -> IO Tag) ->  Text -> IO Text
replacePrImages f file = case (file =~ ("PR-([[:digit:]]+)" :: Text) :: (Text, Text, Text, [Text])) of
                        (pre, "", "", [])   -> pure pre
                        (pre, _, post, [p]) -> let old = read $ unpack p
                                                   new = f old
                                                in append
                                                   <$> new
                                                   <*> replacePrImages f post
                                                   <&> append pre
                        v                   -> error $ "Regex failed me " ++ show v
