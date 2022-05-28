module Presentable.Data.Geometry where

-- An unplaced rectangle, consisting of a width and a height.
data Rect = Rect { rectColumns :: Int, rectRows :: Int } deriving ( Show, Eq )