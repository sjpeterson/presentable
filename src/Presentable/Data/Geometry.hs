module Presentable.Data.Geometry where

-- | An unplaced rectangle, consisting of a width and a height.
data Rect = Rect { rectColumns :: Int, rectRows :: Int } deriving ( Show, Eq )

-- | Shrink a rectangle horizontally.
hShrink :: Rect -> Int -> Rect
hShrink rect k = rect { rectColumns = (rectColumns rect - k) }

-- | Shrink a rectangle vertically.
vShrink :: Rect -> Int -> Rect
vShrink rect k = rect { rectRows = (rectRows rect - k) }

-- | Limit a rectangle to the dimensions provided by another rectangle.
limit :: Rect -> Rect -> Rect
limit limitsRect rect =
    Rect { rectColumns = min (rectColumns limitsRect) (rectColumns rect)
         , rectRows = min (rectRows limitsRect) (rectRows rect)
         }