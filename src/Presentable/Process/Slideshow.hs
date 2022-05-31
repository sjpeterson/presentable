{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Presentable.Process.Slideshow
    ( fitTo
    , fitOneTo
    , wrapAt
    , wrapRelaxedAt
    ) where

import Data.Foldable ( foldr' )
import Data.List.NonEmpty ( NonEmpty ( (:|) ), (<|) )
import qualified Data.List.NonEmpty as NE
import Data.Text ( Text )
import qualified Data.Text as T

import Presentable.Data.Geometry ( Rect ( Rect, rectColumns, rectRows )
                                 , hShrink
                                 , vShrink
                                 )
import Presentable.Data.Slideshow
    ( InlineTextTag ( PlainText )
    , Slide ( SingleContentSlide , TitleSlide )
    , SlideContent ( BulletList, NoContent )
    , TaggedText
    , TextBlock ( unTextBlock )
    , plainTextBlock
    )

type WrappingError = Text

-- | Fit a non-empty list of slides to the given dimensions.
fitTo :: Rect -> NonEmpty Slide -> Either WrappingError (NonEmpty Slide)
fitTo rect slides = foldr' f (fitOneTo rect (NE.last slides)) (NE.init slides)
  where
    f slide fittedSlides = case fitOneTo rect slide of
        Left  err     -> Left err
        Right slides' -> fmap (slides' <>) fittedSlides

-- | Fit a single slide to the given dimensions.
fitOneTo :: Rect -> Slide -> Either WrappingError (NonEmpty Slide)
fitOneTo rect@(Rect {..}) slide = case slide of
    TitleSlide         title subtitle -> Right [slide]
    SingleContentSlide title content  ->
        let titleRows = wrappedHeightAt rectColumns (plainTextBlock title)
        in fmap (fmap (SingleContentSlide title)) <$>
            flip fitContentTo content =<< vShrink rect <$> (1 +) <$> titleRows

-- | Fit slide contents to the given dimensions.
fitContentTo :: Rect
             -> SlideContent
             -> Either WrappingError (NonEmpty SlideContent)
fitContentTo _         NoContent          = Right [NoContent]
fitContentTo rect (BulletList items) = fmap BulletList <$>
    vSplit (hShrink rect 2) items

-- | Vertically split a non-empty list of text blocks to fit the given
-- dimensions.
vSplit :: Rect
       -> NonEmpty TextBlock
       -> Either WrappingError (NonEmpty (NonEmpty TextBlock))
vSplit Rect {..} (b:|bs) = NE.reverse . fmap (NE.reverse . fst) <$>
    foldr' f initial (reverse bs)
  where
    initial = case wrappedHeightAt rectColumns b of
        Left err -> Left err
        Right blockHeight -> if blockHeight > rectRows
            then Left "A text block is too large to fit"
            else Right [([b], blockHeight)]
    f _ error@(Left _) = error
    f block (Right (current@(blocks, currentHeight) :| bs')) =
        case wrappedHeightAt rectColumns block of
            Left err -> Left err
            Right blockHeight ->
                let newHeight = currentHeight + blockHeight
                in if blockHeight > rectRows
                    then Left $ "A text block is too large to fit"
                    else Right $ if newHeight > rectRows
                        then ([block], blockHeight)<|current:|bs'
                        else (block<|blocks, newHeight):|bs'

-- | Compute the height of a text block wrapped at the given number of columns.
wrappedHeightAt :: Int -> TextBlock -> Either WrappingError Int
wrappedHeightAt = fmap (fmap NE.length) . wrapAt

-- | Wrap a text block to the given number of columns. Results in an error if
-- some word is longer than the allowed width.
wrapAt :: Int
       -> TextBlock
       -> Either WrappingError (NonEmpty (NonEmpty TaggedText))
wrapAt c = fmap (fmap unparticles) . wrapParticlesAt c . particles . unTextBlock

-- | Wrap particles of a text block to the given number of columns.
wrapParticlesAt :: Int
                -> NonEmpty TaggedText
                -> Either WrappingError (NonEmpty (NonEmpty TaggedText))
wrapParticlesAt c (p :| ps) = NE.reverse . fmap (NE.reverse . fst) <$>
    foldr' f initial (reverse ps)
  where
    initial = Right [([p], T.length $ fst p)]
    f _        error@(Left _)                          = error
    f particle (Right (current@(particles, n) :| ps')) = if particleLength > c
        then Left $ T.concat ["Particle '", fst particle, "' is too long to fit"]
        else Right $ if newLength > c
            then ([particle], particleLength)<|current:|ps'
            else (particle<|particles, newLength):|ps'
      where
        particleLength = T.length $ fst particle
        newLength = n + 1 + particleLength

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
