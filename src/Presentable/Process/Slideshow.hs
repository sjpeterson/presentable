{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Presentable.Process.Slideshow (fitTo, zipValues) where

import Control.Applicative (liftA2)
import Data.Foldable (foldr')
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)

import Presentable.Data.Block (Block (wrappedHeightAt))
import Presentable.Data.Geometry (
    Rect (Rect, rectColumns, rectRows),
    vShrink,
 )
import Presentable.Data.Slideshow (
    BulletList (BulletList),
    Slide (SingleContentSlide, TitleSlide),
    SlideContent (BulletListContent, NoContent, PlainTextContent),
 )
import Presentable.Data.TextBlock (plainTextBlock)
import Presentable.Data.Wrappable (WrappingError)
import Presentable.Traversals (mapExpandM, splitWhen)

-- | Fit a non-empty list of slides to the given dimensions.
fitTo :: Rect -> Int -> NonEmpty Slide -> Either WrappingError (NonEmpty Slide)
fitTo rect footerHeight = mapExpandM (fitOneTo rect footerHeight)

-- | Fit a single slide to the given dimensions.
fitOneTo :: Rect -> Int -> Slide -> Either WrappingError (NonEmpty Slide)
fitOneTo rect@(Rect{..}) footerHeight slide = case slide of
    TitleSlide title subtitle ->
        let rows =
                liftA2
                    (+)
                    (wrappedHeight title)
                    (fmap (1 +) $ fromMaybe (Right 0) (wrappedHeight <$> subtitle))
         in case rows of
                Left err -> Left err
                Right rows ->
                    if rows > rectRows - 2 * footerHeight
                        then Left "Title slide is too tall to fit"
                        else Right [slide]
    SingleContentSlide title content ->
        let titleRows = wrappedHeight title
         in fmap (fmap (SingleContentSlide title))
                <$> flip fitContentTo content
                =<< vShrink rect
                    <$> (1 + footerHeight +)
                    <$> titleRows
  where
    wrappedHeight = wrappedHeightAt rectColumns . plainTextBlock

-- | Fit slide contents to the given dimensions.
fitContentTo ::
    Rect ->
    SlideContent ->
    Either WrappingError (NonEmpty SlideContent)
fitContentTo _ NoContent = Right [NoContent]
fitContentTo rect (BulletListContent (BulletList items)) =
    fmap (BulletListContent . BulletList) <$> vSplit 0 rect items
fitContentTo rect (PlainTextContent textBlocks) =
    fmap PlainTextContent <$> vSplit 1 rect textBlocks

{- | Vertically split a non-empty list of text blocks to fit the given
dimensions.
-}
vSplit ::
    (Block a) =>
    Int ->
    Rect ->
    NonEmpty a ->
    Either WrappingError (NonEmpty (NonEmpty a))
vSplit padding Rect{..} =
    splitWhen
        (wrappedHeightAt rectColumns)
        (> rectRows)
        ((+) . (+ padding))
        (const "A text block is too large to fit")

-- | Pair each slide with their positional value.
zipValues :: NonEmpty Slide -> NonEmpty (Slide, Int)
zipValues slides = NE.zip slides slideValues
  where
    slideValues = NE.reverse $ foldr' f [0] (reverse $ NE.init slides)
    f slide values@(x :| _) = (slideValue slide + x) <| values

{- | Compute the value of a slide. The value is used to track current position
in a slideshow.
-}
slideValue :: Slide -> Int
slideValue (TitleSlide _ _) = 1
slideValue (SingleContentSlide _ slideContent) = slideContentValue slideContent

{- | Compute the value of some slide content. The value is used to track current
position in slideshow.
-}
slideContentValue :: SlideContent -> Int
slideContentValue (BulletListContent (BulletList items)) = NE.length items
slideContentValue (PlainTextContent paragraphs) = NE.length paragraphs
slideContentValue NoContent = 1
