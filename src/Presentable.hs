-- |
-- Module
-- Copyright
-- License
--
-- Main application module for Presentable

module Presentable
    ( runApp
    ) where

import Control.Exception ( bracket )
import qualified Data.Text.IO as TIO
import System.IO ( stderr )

import Presentable.App.Env ( AppEnv, cleanupEnv, mkEnv )
import Presentable.Config ( getConfig )
import Presentable.Data.Slideshow ( Slideshow )
import Presentable.Parse.Slideshow ( parseSlideshow )
import Presentable.UI.Brick ( runBrick )

runApp :: FilePath -> IO ()
runApp slideshowFile = do
    parsed <- parseSlideshow slideshowFile <$> TIO.readFile slideshowFile
    case parsed of
        Left err -> TIO.hPutStrLn stderr err
        Right slideshow -> do
            config <- getConfig
            case config of
                Left err -> TIO.hPutStrLn stderr err
                Right config -> bracket
                    (mkEnv config slideshow)
                    runAppWithEnv
                    cleanupEnv

runAppWithEnv :: AppEnv -> IO ()
runAppWithEnv = runBrick
