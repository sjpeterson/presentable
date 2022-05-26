{-# LANGUAGE RecordWildCards #-}
-- -- |
-- Module
-- Copyright
-- License
--
-- Read-only application runtime environment for Presentable

module Presentable.App.Env where

import Presentable.Config ( Config ( Config, configMaxDimensions ) )
import Presentable.Data.Slideshow ( Dimensions, Slideshow )

data AppEnv = AppEnv
    { maxDimensions :: Dimensions
    , slideshow :: Slideshow
    }

mkEnv :: Config -> Slideshow -> IO AppEnv
mkEnv Config {..} slideshow = do
    return $ AppEnv {..}
  where
    maxDimensions = configMaxDimensions

cleanupEnv :: AppEnv -> IO ()
cleanupEnv _ = return ()