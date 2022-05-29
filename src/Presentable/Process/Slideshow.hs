{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Presentable.Process.Slideshow ( fitTo, wrapAt, wrapRelaxedAt ) where

import Data.Foldable ( foldr' )
import Data.List.NonEmpty ( NonEmpty ( (:|) ), (<|) )
import qualified Data.List.NonEmpty as NE
import Data.Text ( Text )
import qualified Data.Text as T

import Presentable.Data.Geometry ( Rect ( Rect, rectRows ) )
import Presentable.Data.Slideshow ( InlineTextTag ( PlainText )
                                  , Slide ( SingleContentSlide
                                          , TitleSlide
                                          )
                                  , TaggedText
                                  , TextBlock ( unTextBlock )
                                  )

type WrappingError = Text

-- | Fit a non-empty list of slides to the given dimensions.
fitTo :: Rect -> NonEmpty Slide -> Either WrappingError (NonEmpty Slide)
fitTo rect slides = foldr' f lastSlides (NE.init slides)
  where
    f :: Slide -> Either WrappingError (NonEmpty Slide) -> Either WrappingError (NonEmpty Slide)
    f slide slides = (flip (<>) slides) (fitOneTo rect slide)
    lastSlides = fitOneTo rect $ NE.last slides

-- | Fit a single slide to the given dimensions.
fitOneTo :: Rect -> Slide -> Either WrappingError (NonEmpty Slide)
fitOneTo Rect {..} slide = case slide of
    TitleSlide         title subtitle -> Right [slide]
    SingleContentSlide title content  -> Right [slide]

-- | Compute the height of a slide at the given width.
heightAtWidth :: Int -> Slide -> Int
heightAtWidth columns slide = case slide of
    (TitleSlide         _ _) -> 0
    (SingleContentSlide _ _) -> undefined

-- | Wrap a text block to the given number of columns. Results in an error if
-- some word is longer than the allowed width.
wrapAt :: Int -> TextBlock -> Either WrappingError [NonEmpty TaggedText]
wrapAt c = fmap (map unparticles) . wrapParticlesAt c . particles . unTextBlock

-- | Wrap particles of a text block to the given number of columns.
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

-- | Wrap a text block to the given number of columns.
wrapRelaxedAt :: Int -> TextBlock -> [NonEmpty TaggedText]
wrapRelaxedAt c =
    map unparticles . wrapParticlesRelaxedAt c . particles . unTextBlock

-- | Wrap particles of a text block to the given number of columns.
wrapParticlesRelaxedAt :: Int -> NonEmpty TaggedText -> [NonEmpty TaggedText]
wrapParticlesRelaxedAt c (p :| ps) = reverse . map (NE.reverse . fst) $
    foldr' f initial (reverse ps)
  where
    initial = [(p :| [], T.length $ fst p)]
    f t acc = case acc of
        []                         -> [newRow]
        current@(particles, n):ps' ->
            let newLength = n + 1 + pLength
            in if newLength > c
                then newRow:current:ps'
                else (t<|particles, newLength):ps'
      where
        newRow = (t:|[], pLength)
        pLength = T.length $ fst t

-- | Split a non-empty list of tagged inline text parts into particles.
particles :: NonEmpty TaggedText -> NonEmpty TaggedText
particles ((s, PlainText) :| ts) = case ts of
    (x:xs) -> headParticles <> particles (x:|xs)
    _      -> headParticles
  where
    headParticles = case map (flip (,) PlainText) (T.words s) of
        (w:ws) -> w :| ws
        _      -> (s, PlainText) :| []

-- | Join particles where allowed by their tags.
unparticles :: NonEmpty TaggedText -> NonEmpty TaggedText
unparticles ps@(_ :| []) = ps
unparticles ((s1, PlainText) :| ((s2, PlainText):ps)) =
    unparticles $ (T.unwords [s1, s2], PlainText) :| ps
