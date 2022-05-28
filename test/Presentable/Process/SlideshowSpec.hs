{-# LANGUAGE OverloadedStrings #-}

module Presentable.Process.SlideshowSpec where

import Data.Either ( isLeft )
import Data.List.NonEmpty ( NonEmpty ( (:|) ) )

import Test.Hspec ( Spec, describe, it, shouldBe, shouldSatisfy )

import Presentable.Data.Slideshow ( InlineTextTag ( PlainText )
                                  , TextBlock ( TextBlock )
                                  , plainTextBlock
                                  )
import Presentable.Process.Slideshow ( wrapAt )


spec :: Spec
spec = do
    describe "wrapAt" $ do
        it "leaves a short enough plain text line intact" $ do
            wrapAt 10 (plainTextBlock "Short text") `shouldBe`
                Right [("Short text", PlainText) :| []]
        it "wraps plain text on exact word boundary" $ do
            wrapAt 5 (plainTextBlock "Short text") `shouldBe`
                Right [("Short", PlainText) :| [], ("text", PlainText) :| []]
        it "wraps plain text on preceeding word boundary" $ do
            wrapAt 7 (plainTextBlock "Short text") `shouldBe`
                Right [("Short", PlainText) :| [], ("text", PlainText) :| []]
        it "wraps from the beginning" $ do
            wrapAt 18 (plainTextBlock "Slightly longer text") `shouldBe`
                Right [ ("Slightly longer", PlainText) :| []
                      , ("text", PlainText) :| []
                      ]
        it "fails if a word is too long to fit" $ do
            wrapAt 10 (plainTextBlock "I have exceedingly long words")
                `shouldSatisfy` isLeft
