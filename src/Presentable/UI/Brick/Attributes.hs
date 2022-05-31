{-# LANGUAGE OverloadedStrings #-}

module Presentable.UI.Brick.Attributes where

import Brick ( AttrName )

-- | Brick attribute name.
titleAttr, bulletAttr, errorAttr :: AttrName
titleAttr = "titleAttr"
bulletAttr = "bulletAttr"
errorAttr = "errorAttr"
