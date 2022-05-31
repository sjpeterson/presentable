{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Presentable.Process.SlideshowSpec where

import Data.Either ( isLeft )
import Data.List.NonEmpty ( NonEmpty ( (:|) ) )

import Test.Hspec ( Spec, describe, it, shouldBe, shouldSatisfy )

import Presentable.Data.Geometry ( Rect ( Rect ) )
import Presentable.Data.Slideshow ( InlineTextTag ( PlainText )
                                  , Slide ( SingleContentSlide )
                                  , SlideContent ( BulletList )
                                  , TextBlock ( TextBlock )
                                  , plainTextBlock
                                  )
import Presentable.Process.Slideshow ( fitOneTo, wrapAt, wrapRelaxedAt )


spec :: Spec
spec = do
    describe "wrapAt" $ do
        it "leaves a short enough plain text line intact" $ do
            wrapAt 10 (plainTextBlock "Short text") `shouldBe`
                Right [[("Short text", PlainText)]]
        it "wraps plain text on exact word boundary" $ do
            wrapAt 5 (plainTextBlock "Short text") `shouldBe`
                Right [[("Short", PlainText)], [("text", PlainText)]]
        it "wraps plain text on preceeding word boundary" $ do
            wrapAt 7 (plainTextBlock "Short text") `shouldBe`
                Right [[("Short", PlainText)], [("text", PlainText)]]
        it "wraps from the beginning" $ do
            wrapAt 18 (plainTextBlock "Slightly longer text") `shouldBe`
                Right [ [("Slightly longer", PlainText)]
                      , [("text", PlainText)]
                      ]
        it "fails if a word is too long to fit" $ do
            wrapAt 10 (plainTextBlock "I have exceedingly long words")
                `shouldSatisfy` isLeft
    describe "wrapRelaxedAt" $ do
        it "leaves a short enough plain text line intact" $ do
            wrapRelaxedAt 10 (plainTextBlock "Short text") `shouldBe`
                [[("Short text", PlainText)]]
        it "wraps plain text on exact word boundary" $ do
            wrapRelaxedAt 5 (plainTextBlock "Short text") `shouldBe`
                [[("Short", PlainText)], [("text", PlainText)]]
        it "wraps plain text on preceeding word boundary" $ do
            wrapRelaxedAt 7 (plainTextBlock "Short text") `shouldBe`
                [("Short", PlainText) :| [], ("text", PlainText) :| []]
        it "wraps from the beginning" $ do
            wrapRelaxedAt 18 (plainTextBlock "Slightly longer text") `shouldBe`
                [ [("Slightly longer", PlainText)]
                , [("text", PlainText)]
                ]
        it "allows individual words to exceed the line length" $ do
            wrapRelaxedAt 10 (plainTextBlock "I have exceedingly long words")
                `shouldBe` [ [("I have", PlainText)]
                           , [("exceedingly", PlainText)]
                           , [("long words", PlainText)]
                           ]
    describe "fitOneTo" $ do
        it "splits a long bullet list leaving items intact" $ do
            fitOneTo (Rect 12 7) testBulletListSlide `shouldBe`
                Right [ SingleContentSlide
                            "Slide Title"
                            (BulletList [ plainTextBlock "First item"
                                        , plainTextBlock "Second item"
                                        , plainTextBlock "Third item"
                                        ])
                      , SingleContentSlide
                            "Slide Title"
                            (BulletList [ plainTextBlock "Fourth item" ])
                      ]
  where
    testBulletListSlide = SingleContentSlide
        "Slide Title"
        (BulletList [ plainTextBlock "First item"
                    , plainTextBlock "Second item"
                    , plainTextBlock "Third item"
                    , plainTextBlock "Fourth item"
                    ])
