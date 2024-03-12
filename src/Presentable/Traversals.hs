{-# LANGUAGE OverloadedLists #-}

module Presentable.Traversals where

import Data.Foldable (foldr')
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NE

-- | Conditionally expand a non-empty list.
mapExpandM :: (Monad m) => (a -> m (NonEmpty c)) -> NonEmpty a -> m (NonEmpty c)
mapExpandM f xs = foldr' f' (f $ NE.last xs) (NE.init xs)
  where
    f' = (=<<) . flip (fmap . flip (<>)) . f

-- | Split a non-empty list on some condition.
splitWhen ::
    (a -> Either b c) ->
    (c -> Bool) ->
    (c -> c -> c) ->
    (a -> b) ->
    NonEmpty a ->
    Either b (NonEmpty (NonEmpty a))
splitWhen valueOf c agg fail (x :| xs) =
    NE.reverse . fmap (NE.reverse . fst)
        <$> foldr' ((=<<) . f) initial (reverse xs)
  where
    initial =
        valueOf x >>= \initV ->
            if c initV
                then Left (fail x)
                else Right [([x], initV)]
    f y (current@(ys, currentV) :| rest) =
        valueOf y >>= \nextV ->
            if c nextV
                then Left $ fail y
                else
                    Right $
                        let newV = agg currentV nextV
                         in if c newV
                                then ([y], nextV) <| current :| rest
                                else (y <| ys, newV) :| rest
