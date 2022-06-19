module Presentable.Data.Wrappable where

import Data.List.NonEmpty ( NonEmpty )
import Data.Text ( Text )

type WrappingError = Text

class Wrappable a where
    wrapAt :: Int -> a -> Either WrappingError (NonEmpty a)
    wrapRelaxedAt :: Int -> a -> NonEmpty a