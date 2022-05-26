module Main where

import Options.Applicative ( Parser
                           , (<**>)
                           , argument
                           , execParser
                           , header
                           , helper
                           , info
                           , fullDesc
                           , metavar
                           , progDesc
                           , str
                           )

import Presentable ( runApp )

data Options = Options { optionsSlideshowFile :: FilePath }

options :: Parser Options
options = Options <$> argument str (metavar "FILE")

main :: IO ()
main = runApp . optionsSlideshowFile =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Run a presentation in the terminal"
     <> header "presentable - lightweight presentations" )

