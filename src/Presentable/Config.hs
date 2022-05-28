module Presentable.Config
    ( Config ( Config, configMaxDimensions )
    , getConfig
    ) where

import Data.Text ( Text )

import Presentable.Data.Geometry
    ( Rect ( Rect, rectColumns, rectRows ) )

-- | A type alias for config errors.
type ConfigError = Text

-- | Config data type.
data Config
  = Config { configMaxDimensions :: Rect }

-- | The default config.
defaultConfig :: Config
defaultConfig = Config { configMaxDimensions = defaultDimensions }
  where
    defaultDimensions = Rect { rectColumns = 80
                             , rectRows = 38
                             }

-- | A function to read config from file.
getConfig :: IO (Either ConfigError Config)
getConfig = return $ Right defaultConfig