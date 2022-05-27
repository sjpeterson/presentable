{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module     : Presentable.Process.Slideshow
-- Copyright  : Stefan Peterson
-- License    : MIT
--
-- Slideshow processing functions

module Presentable.Process.Slideshow ( fitTo, wrapTo ) where

import Data.Bifunctor ( first, second )
import Data.Foldable ( foldr' )
import Data.List.NonEmpty ( NonEmpty ( (:|) ) )
import qualified Data.List.NonEmpty as NE ( init, last )
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import qualified Data.Text as T

import Presentable.Data.Buffer ( Buffer )
import Presentable.Data.Geometry ( Rect ( Rect, rectRows ) )
import Presentable.Data.Slideshow ( Slide ( ErrorSlide
                                          , SingleContentSlide
                                          , TitleSlide
                                          )
                                  , Slideshow
                                  , TextBlock ( TextBlock )
                                  , InlineText ( PlainText )
                                  )

-- | Fit a non-empty list of slides to the given dimensions.
fitTo :: Rect -> NonEmpty Slide -> NonEmpty Slide
fitTo rect slides =
    fromMaybe sizeErrorSlide $ foldr' f lastSlides (NE.init slides)
  where
    f _     Nothing       = Nothing
    f slide (Just slides) = case (fitOneTo rect slide) of
        Nothing      -> Nothing
        Just slides' -> Just $ slides' <> slides
    lastSlides = fitOneTo rect $ NE.last slides

fitOneTo :: Rect -> Slide -> Maybe (NonEmpty Slide)
fitOneTo Rect {..} slide = case slide of
    TitleSlide         title subtitle -> Just $ slide :| []
    SingleContentSlide title content  -> Just $ slide :| []
    ErrorSlide         _              -> Just $ slide :| []

heightAtWidth :: Int -> Slide -> Int
heightAtWidth columns slide = case slide of
    (TitleSlide         _ _) -> 0
    (SingleContentSlide _ _) -> undefined
    (ErrorSlide         _  ) -> undefined

sizeErrorSlide :: NonEmpty Slide
sizeErrorSlide = (ErrorSlide "The window is too small for this slideshow") :| []

-- | Wrap a text block to the given number of columns.
wrapTo :: Int -> TextBlock -> Either WrappingError [[InlineText]]
wrapTo columns (TextBlock segments) = undefined

type WrappingError = Text

-- | Wrap plain text at the given column
wrapTextAt :: Int -> Text -> Either WrappingError [Text]
wrapTextAt columns s
    | T.length firstWord > columns = tooLongError
    | strippedLine == ""           = Right []
    | otherwise                    = (strippedLine:) <$> (wrapTextAt columns joined)
  where
    ((line, spill), remaining) =
        first (T.breakOnEnd " ") $ T.splitAt (columns + 1) s
    strippedLine = T.strip line
    joined = T.append spill remaining
    firstWord = case T.words line of
        []    -> ""
        (w:_) -> w
    tooLongError = Left $ T.concat ["Word '", firstWord, "' is too long to fit"]
