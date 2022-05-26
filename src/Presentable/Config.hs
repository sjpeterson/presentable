-- |
-- Module     : Presentable.Config
-- Copyright  : 2022 Stefan Peterson
-- License    : MIT
--
-- Configuration for Presentable

module Presentable.Config
    ( Config ( Config, configMaxDimensions )
    , getConfig
    ) where

import Data.Text ( Text )

import Presentable.Data.Geometry
    ( Rect ( Rect, rectColumns, rectRows ) )

type ConfigError = Text

data Config
  = Config { configMaxDimensions :: Rect }

defaultConfig :: Config
defaultConfig = Config { configMaxDimensions = defaultDimensions }
  where
    defaultDimensions = Rect { rectColumns = 80
                             , rectRows = 38
                             }

getConfig :: IO (Either ConfigError Config)
getConfig = return $ Right defaultConfig