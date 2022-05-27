{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module     : Presentable.Process.Slideshow
-- Copyright  : Stefan Peterson
-- License    : MIT
--
-- Slideshow processing functions

module Presentable.Process.Slideshow ( fitTo ) where

import Data.Foldable ( foldr' )
import Data.List.NonEmpty ( NonEmpty ( (:|) ) )
import qualified Data.List.NonEmpty as NE ( init, last )
import Data.Maybe ( fromMaybe )

import Presentable.Data.Buffer ( Buffer )
import Presentable.Data.Geometry ( Rect ( Rect, rectRows ) )
import Presentable.Data.Slideshow ( Slide ( ErrorSlide
                                          , SingleContentSlide
                                          , TitleSlide
                                          )
                                  , Slideshow
                                  )

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