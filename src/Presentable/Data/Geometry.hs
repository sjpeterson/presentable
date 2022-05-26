-- |
-- Module     : Presentable.Data.Geometry
-- Copyright  : Stefan Peterson
-- License    : MIT
--
-- Geometry data types

module Presentable.Data.Geometry where

data Rect = Rect { rectColumns :: Int, rectRows :: Int } deriving ( Show, Eq )