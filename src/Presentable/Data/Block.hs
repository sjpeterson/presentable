module Presentable.Data.Block where

import Presentable.Data.Wrappable (WrappingError)

class Block a where
    wrappedHeightAt :: Int -> a -> Either WrappingError Int
