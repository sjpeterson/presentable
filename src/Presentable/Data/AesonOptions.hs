module Presentable.Data.AesonOptions where

import Data.Aeson.TH ( Options ( fieldLabelModifier ), defaultOptions )
import Data.Char ( toLower )

stripPrefix :: Int -> Options
stripPrefix n = defaultOptions { fieldLabelModifier = firstToLower . drop n }
  where
    firstToLower (c:cs) = (toLower c):cs
    firstToLower _      = ""
