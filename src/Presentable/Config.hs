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

import Presentable.Data.Slideshow
    ( Dimensions ( Dimensions, dimensionsColumns, dimensionsRows ) )

type ConfigError = Text

data Config
  = Config { configMaxDimensions :: Dimensions }

defaultConfig :: Config
defaultConfig = Config { configMaxDimensions = defaultDimensions }
  where
    defaultDimensions = Dimensions { dimensionsColumns = 80
                                   , dimensionsRows = 38
                                   }

getConfig :: IO (Either ConfigError Config)
getConfig = return $ Right defaultConfig