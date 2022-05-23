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

data Options = Options { optionsPresentationFile :: FilePath }

options :: Parser Options
options = Options <$> argument str (metavar "FILE")

main :: IO ()
main = runApp . optionsPresentationFile =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Run a presentation in the terminal"
     <> header "presentable - lightweight presentations" )

