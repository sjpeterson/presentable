-- |
-- Module     : Presentable.Data.Buffer
-- Copyright  : 2022 Stefan Peterson
-- License    : MIT
--
-- Generic buffer type

module Presentable.Data.Buffer where

data Buffer a = Buffer
    { bufferCurrent :: a
    , bufferNext :: [a]
    , bufferPrevious :: [a]
    } deriving ( Eq, Show )

next :: Buffer a -> Buffer a
next buffer@(Buffer _ [] _ ) = buffer
next (Buffer x (nextX:xs) prevXs) = Buffer nextX xs (x:prevXs)

prev :: Buffer a -> Buffer a
prev buffer@(Buffer _ _ []) = buffer
prev (Buffer x nextXs (prevX:prevXs)) = Buffer prevX (x:nextXs) prevXs