-- |
-- Module
-- Copyright
-- License
--
-- Main application module for Presentable

module Presentable
    ( runApp
    ) where

import qualified Data.Text.IO as TIO
import System.IO ( stderr )

import Presentable.Parse.Presentation ( parsePresentation )
import Presentable.UI.Brick ( runBrick )

runApp :: FilePath -> IO ()
runApp presentationFile = do
    parsed <- parsePresentation presentationFile <$> TIO.readFile presentationFile
    case parsed of
        Left err -> TIO.hPutStrLn stderr err
        Right presentation -> runBrick presentation
