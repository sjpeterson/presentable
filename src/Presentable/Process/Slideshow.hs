{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module     : Presentable.Process.Slideshow
-- Copyright  : Stefan Peterson
-- License    : MIT
--
-- Slideshow processing functions

module Presentable.Process.Slideshow ( fitTo, wrapAt ) where

import Data.Bifunctor ( first, second )
import Data.Foldable ( foldr' )
import Data.List.NonEmpty ( NonEmpty ( (:|) ), (<|) )
import qualified Data.List.NonEmpty as NE
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import qualified Data.Text as T

import Presentable.Data.Buffer ( Buffer )
import Presentable.Data.Geometry ( Rect ( Rect, rectRows ) )
import Presentable.Data.Slideshow ( InlineTextTag ( PlainText )
                                  , Slide ( ErrorSlide
                                          , SingleContentSlide
                                          , TitleSlide
                                          )
                                  , Slideshow
                                  , TaggedText
                                  , TextBlock ( TextBlock, unTextBlock )
                                  )

type WrappingError = Text

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
wrapAt :: Int -> TextBlock -> Either WrappingError [NonEmpty TaggedText]
wrapAt c = fmap (map unparticles) . wrapParticlesAt c . particles . unTextBlock

wrapParticlesAt :: Int
                -> NonEmpty TaggedText
                -> Either WrappingError [NonEmpty TaggedText]
wrapParticlesAt c (p :| ps) = reverse . map (NE.reverse . fst) <$>
    foldr' f initial (reverse ps)
  where
    initial = Right [(p :| [], T.length $ fst p)]
    f _ error@(Left _) = error
    f t (Right acc)    = if pLength > c
        then Left $ T.concat ["Particle '", fst t, "' is too long to fit"]
        else Right $ case acc of
            []                         -> [newRow]
            current@(particles, n):ps' ->
              let newLength = n + 1 + pLength
              in if newLength > c
                then newRow:current:ps'
                else (t<|particles, newLength):ps'
      where
        newRow = (t:|[], pLength)
        pLength = T.length $ fst t

particles :: NonEmpty TaggedText -> NonEmpty TaggedText
particles ((s, PlainText) :| ts) = case ts of
    (x:xs) -> headParticles <> particles (x:|xs)
    _      -> headParticles
  where
    headParticles = case map (flip (,) PlainText) (T.words s) of
        (w:ws) -> w :| ws
        _      -> (s, PlainText) :| []

unparticles :: NonEmpty TaggedText -> NonEmpty TaggedText
unparticles ps@(p :| []) = ps
unparticles ((s1, PlainText) :| ((s2, PlainText):ps)) =
    unparticles $ (T.unwords [s1, s2], PlainText) :| ps
