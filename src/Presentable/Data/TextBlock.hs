{-# LANGUAGE OverloadedStrings #-}

module Presentable.Data.TextBlock (
    InlineTextTag (PlainText),
    TaggedText,
    TextBlock (TextBlock, unTextBlock),
    plainTextBlock,
) where

import Data.Foldable (foldr')
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T

import Presentable.Data.Block (Block (wrappedHeightAt))
import Presentable.Data.Wrappable (
    Wrappable (wrapAt, wrapRelaxedAt),
    WrappingError,
 )
import Presentable.Traversals (splitWhen)

{- | A newtype for blocks of text that must not be split. A non-empty list of
tagged inline text parts.
-}
newtype TextBlock = TextBlock
    { unTextBlock :: NonEmpty TaggedText
    }
    deriving (Eq, Show)

instance Wrappable TextBlock where
    wrapAt c = reTextBlocks . wrapParticlesAt c . particles . unTextBlock
      where
        reTextBlocks = fmap $ fmap $ TextBlock . unparticles

    wrapRelaxedAt c =
        reTextBlocks . wrapParticlesRelaxedAt c . particles . unTextBlock
      where
        reTextBlocks = fmap $ TextBlock . unparticles

instance Block TextBlock where
    wrappedHeightAt = fmap (fmap NE.length) . wrapAt

-- | A type alias for some inline text and a tag describing its type.
type TaggedText = (Text, InlineTextTag)

-- | The different types of inline text.
data InlineTextTag = PlainText
    deriving (Eq, Show)

-- | Create a TextBlock from some Text.
plainTextBlock :: Text -> TextBlock
plainTextBlock = TextBlock . (:| []) . (flip (,) PlainText)

-- | Split a non-empty list of tagged inline text parts into particles.
particles :: NonEmpty TaggedText -> NonEmpty TaggedText
particles ((s, PlainText) :| ts) = case ts of
    (x : xs) -> headParticles <> particles (x :| xs)
    _ -> headParticles
  where
    headParticles = case map (flip (,) PlainText) (T.words s) of
        (w : ws) -> w :| ws
        _ -> (s, PlainText) :| []

-- | Join particles where allowed by their tags.
unparticles :: NonEmpty TaggedText -> NonEmpty TaggedText
unparticles ps@(_ :| []) = ps
unparticles ((s1, PlainText) :| ((s2, PlainText) : ps)) =
    unparticles $ (T.unwords [s1, s2], PlainText) :| ps

-- | Wrap particles of a text block to the given number of columns.
wrapParticlesAt ::
    Int ->
    NonEmpty TaggedText ->
    Either WrappingError (NonEmpty (NonEmpty TaggedText))
wrapParticlesAt c = splitWhen (Right . T.length . fst) (> c) ((+) . (1 +)) fail
  where
    fail p = T.concat ["Particle '", fst p, "' is too long to fit"]

-- | Wrap particles of a text block to the given number of columns.
wrapParticlesRelaxedAt ::
    Int ->
    NonEmpty TaggedText ->
    NonEmpty (NonEmpty TaggedText)
wrapParticlesRelaxedAt c (p :| ps) =
    NE.reverse . fmap (NE.reverse . fst) $
        foldr' f initial (reverse ps)
  where
    initial = (p :| [], T.length $ fst p) :| []
    f t acc = case acc of
        current@(particles, n) :| ps' ->
            let newLength = n + 1 + pLength
             in if newLength > c
                    then newRow <| current :| ps'
                    else (t <| particles, newLength) :| ps'
      where
        newRow = (t :| [], pLength)
        pLength = T.length $ fst t
