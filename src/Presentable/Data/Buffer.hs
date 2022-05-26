-- |
-- Module     : Presentable.Data.Buffer
-- Copyright  : 2022 Stefan Peterson
-- License    : MIT
--
-- Generic buffer type

module Presentable.Data.Buffer where

import Data.List.NonEmpty ( NonEmpty )
import qualified Data.List.NonEmpty as NE

data Buffer a = Buffer
    { bufferCurrent :: a
    , bufferNext :: [a]
    , bufferPrevious :: [a]
    } deriving ( Eq, Show )

bufferOf :: NonEmpty a -> Buffer a
bufferOf xs = Buffer (NE.head xs) (NE.tail xs) []

next :: Buffer a -> Buffer a
next buffer@(Buffer _ [] _ ) = buffer
next (Buffer x (nextX:xs) prevXs) = Buffer nextX xs (x:prevXs)

prev :: Buffer a -> Buffer a
prev buffer@(Buffer _ _ []) = buffer
prev (Buffer x nextXs (prevX:prevXs)) = Buffer prevX (x:nextXs) prevXs