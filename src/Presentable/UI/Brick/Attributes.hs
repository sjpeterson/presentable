{-# LANGUAGE OverloadedStrings #-}

module Presentable.UI.Brick.Attributes where

import Brick ( AttrName )

-- | Brick attribute name.
titleAttr, subtitleAttr, bulletAttr, errorAttr :: AttrName
titleAttr = "titleAttr"
subtitleAttr = "subtitleAttr"
bulletAttr = "bulletAttr"
errorAttr = "errorAttr"
