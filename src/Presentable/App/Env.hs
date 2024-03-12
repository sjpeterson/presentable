module Presentable.App.Env where

import Lens.Micro ((^.))

import Presentable.Data.Config (
    Config,
    Styles,
    configMaxDimensions,
    configStyles,
 )
import Presentable.Data.Geometry (Rect)
import Presentable.Data.Slideshow (Slideshow)

-- | Read-only runtime environment.
data AppEnv = AppEnv
    { maxDimensions :: Rect
    , slideshow :: Slideshow
    , styles :: Styles
    }

-- | Create an environment.
mkEnv :: Config -> Slideshow -> IO AppEnv
mkEnv config slideshow =
    return
        AppEnv
            { maxDimensions = config ^. configMaxDimensions
            , slideshow = slideshow
            , styles = config ^. configStyles
            }

-- | Clean up the environment.
cleanupEnv :: AppEnv -> IO ()
cleanupEnv _ = return ()
