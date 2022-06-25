{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Presentable.Data.TextBlockSpec where

import Data.Either ( isLeft )
import Data.List.NonEmpty ( NonEmpty ( (:|) ) )
import Test.Hspec ( Spec, describe, it, shouldBe, shouldSatisfy )

import Presentable.Data.TextBlock ( plainTextBlock )
import Presentable.Data.Wrappable ( wrapAt, wrapRelaxedAt )

spec :: Spec
spec = do
    describe "wrapAt" $ do
        it "leaves a short enough plain text line intact" $ do
            wrapAt 10 (plainTextBlock "Short text") `shouldBe`
                Right [plainTextBlock "Short text"]
        it "wraps plain text on exact word boundary" $ do
            wrapAt 5 (plainTextBlock "Short text") `shouldBe`
                Right [plainTextBlock "Short", plainTextBlock "text"]
        it "wraps plain text on preceeding word boundary" $ do
            wrapAt 7 (plainTextBlock "Short text") `shouldBe`
                Right [plainTextBlock "Short", plainTextBlock "text"]
        it "wraps from the beginning" $ do
            wrapAt 18 (plainTextBlock "Slightly longer text") `shouldBe`
                Right [plainTextBlock "Slightly longer", plainTextBlock "text"]
        it "fails if a word is too long to fit" $ do
            wrapAt 10 (plainTextBlock "I have exceedingly long words")
                `shouldSatisfy` isLeft
    describe "wrapRelaxedAt" $ do
        it "leaves a short enough plain text line intact" $ do
            wrapRelaxedAt 10 (plainTextBlock "Short text") `shouldBe`
                [plainTextBlock "Short text"]
        it "wraps plain text on exact word boundary" $ do
            wrapRelaxedAt 5 (plainTextBlock "Short text") `shouldBe`
                [plainTextBlock "Short", plainTextBlock "text"]
        it "wraps plain text on preceeding word boundary" $ do
            wrapRelaxedAt 7 (plainTextBlock "Short text") `shouldBe`
                [plainTextBlock "Short", plainTextBlock "text"]
        it "wraps from the beginning" $ do
            wrapRelaxedAt 18 (plainTextBlock "Slightly longer text") `shouldBe`
                [plainTextBlock "Slightly longer", plainTextBlock "text"]
        it "allows individual words to exceed the line length" $ do
            wrapRelaxedAt 10 (plainTextBlock "I have exceedingly long words")
                `shouldBe` [ plainTextBlock "I have"
                           , plainTextBlock "exceedingly"
                           , plainTextBlock "long words"
                           ]
