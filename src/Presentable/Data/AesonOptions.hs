module Presentable.Data.AesonOptions where

import Data.Aeson.TH ( Options ( fieldLabelModifier ), defaultOptions )
import Data.Char ( toLower )

-- | Drop the first n characters and set the new leading character to lowercase.
stripPrefix :: Int -> Options
stripPrefix n = defaultOptions { fieldLabelModifier = firstToLower . drop n }
  where
    firstToLower (c:cs) = (toLower c):cs
    firstToLower _      = ""
