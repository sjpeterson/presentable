{-# LANGUAGE RecordWildCards #-}

module Main where

import Paths_presentable ( version )
import Data.Version ( showVersion )
import Options.Applicative ( Parser
                           , (<**>)
                           , (<|>)
                           , argument
                           , execParser
                           , header
                           , helper
                           , info
                           , flag
                           , flag'
                           , fullDesc
                           , long
                           , metavar
                           , progDesc
                           , short
                           , str
                           , value
                           )

import Presentable ( runApp )

data Options = PrintVersionOptions ()
             | SlideshowOptions Bool FilePath

printVersionOptions :: Parser Options
printVersionOptions = PrintVersionOptions
    <$> flag' () (long "version" <> short 'v')

slideshowOptions :: Parser Options
slideshowOptions = SlideshowOptions
    <$> flag False True (long "check" <> short 'c')
    <*> argument str (metavar "FILE" <> value "")

options :: Parser Options
options = printVersionOptions <|> slideshowOptions

main :: IO ()
main = runApp' =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Run a presentation in the terminal"
     <> header "presentable - lightweight presentations" )

    runApp' (SlideshowOptions check slideshowFile) =
        runApp check slideshowFile
    runApp' (PrintVersionOptions _) =
        putStrLn $ unwords ["Presentable", showVersion version]
