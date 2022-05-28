{-# LANGUAGE RecordWildCards #-}

module Presentable.App.Env where

import Presentable.Config ( Config ( Config, configMaxDimensions ) )
import Presentable.Data.Geometry ( Rect )
import Presentable.Data.Slideshow ( Slideshow )

-- | Read-only runtime environment.
data AppEnv = AppEnv
    { maxDimensions :: Rect
    , slideshow :: Slideshow
    }

-- | Create an environment.
mkEnv :: Config -> Slideshow -> IO AppEnv
mkEnv Config {..} slideshow = do
    return $ AppEnv {..}
  where
    maxDimensions = configMaxDimensions

-- | Clean up the environment.
cleanupEnv :: AppEnv -> IO ()
cleanupEnv _ = return ()