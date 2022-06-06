module Presentable.Data.Buffer where

import Data.List.NonEmpty ( NonEmpty )
import qualified Data.List.NonEmpty as NE

-- | Buffer type. A buffer is a non-empty list that extends both forwards and
-- backwards from the active item.
data Buffer a = Buffer
    { bufferCurrent :: a
    , bufferNext :: [a]
    , bufferPrevious :: [a]
    } deriving ( Eq, Show )

-- | Create a buffer from a non-empty list.
bufferOf :: NonEmpty a -> Buffer a
bufferOf xs = Buffer (NE.head xs) (NE.tail xs) []

-- | Go to the next item in a buffer.
next :: Buffer a -> Buffer a
next buffer@(Buffer _ [] _ ) = buffer
next (Buffer x (nextX:xs) prevXs) = Buffer nextX xs (x:prevXs)

-- | Go to the previous item in a buffer.
prev :: Buffer a -> Buffer a
prev buffer@(Buffer _ _ []) = buffer
prev (Buffer x nextXs (prevX:prevXs)) = Buffer prevX (x:nextXs) prevXs
