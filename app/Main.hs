module Main (main) where


import           Lib
import           Options.Applicative

main :: IO ()
main = run =<< execParser opts
        where
            opts = info (cmdOpts <**> helper)
              ( fullDesc
             <> progDesc "Replace PR images in Kyma" )
