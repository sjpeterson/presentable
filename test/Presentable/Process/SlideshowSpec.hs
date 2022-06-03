{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Presentable.Process.SlideshowSpec where

import Data.Either ( isLeft )
import Data.List.NonEmpty ( NonEmpty ( (:|) ) )

import Test.Hspec ( Spec, describe, it, shouldBe, shouldSatisfy )

import Presentable.Data.Geometry ( Rect ( Rect ) )
import Presentable.Data.Slideshow ( InlineTextTag ( PlainText )
                                  , Slide ( SingleContentSlide, TitleSlide )
                                  , SlideContent ( BulletList )
                                  , TextBlock ( TextBlock )
                                  , plainTextBlock
                                  )
import Presentable.Process.Slideshow ( fitTo, wrapAt, wrapRelaxedAt, zipValues )


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
    describe "zipValues" $ do
        it "pairs the first slide with position 0" $ do
            zipValues [testTitleSlide] `shouldBe` [(testTitleSlide, 0)]
            zipValues [testBulletListSlide] `shouldBe`
                [(testBulletListSlide, 0)]
        it "zips with a cumulative sum of values" $ do
            zipValues [testTitleSlide, testBulletListSlide, testBulletListSlide]
                `shouldBe` [ (testTitleSlide, 0)
                           , (testBulletListSlide, 1)
                           , (testBulletListSlide, 5)
                           ]
    describe "fitTo" $ do
        it "leaves a small enough title slide intact" $ do
            fitTo (Rect 12 4) 0 [testTitleSlide] `shouldBe`
                Right [testTitleSlide]
        it "fails if a title slide is too large" $ do
            fitTo (Rect 12 3) 0 [testTitleSlide] `shouldSatisfy` isLeft
        it "deducts footer height on both sides of a title slide" $ do
            fitTo (Rect 12 7) 2 [testTitleSlide] `shouldSatisfy` isLeft
        it "splits a long bullet list leaving items intact" $ do
            fitTo (Rect 12 7) 0 [testBulletListSlide] `shouldBe`
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
        it "deducts footer height at one end of a bullet list slide" $ do
            fitTo (Rect 12 9) 2 [testBulletListSlide] `shouldBe`
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
        it "fails if a bullet list item is too large to fit" $ do
            fitTo (Rect 12 7) 0 [testLongItemBulletListSlide] `shouldSatisfy`
                isLeft
        it "fails if the first bullet list item is too large to fit" $ do
            fitTo (Rect 12 7) 0 [testLongFirstItemBulletListSlide] `shouldSatisfy`
                isLeft
  where
    testTitleSlide = TitleSlide "Presentation" (Just "With a subtitle")
    testBulletListSlide = SingleContentSlide
        "Slide Title"
        (BulletList [ plainTextBlock "First item"
                    , plainTextBlock "Second item"
                    , plainTextBlock "Third item"
                    , plainTextBlock "Fourth item"
                    ])
    testLongItemBulletListSlide = SingleContentSlide
        "Slide Title"
        (BulletList
            [ plainTextBlock "First item"
            , plainTextBlock "A long item that does not fit in the space given"
            , plainTextBlock "Third item"
            ])
    testLongFirstItemBulletListSlide = SingleContentSlide
        "Slide Title"
        (BulletList
            [ plainTextBlock "A long item that does not fit in the space given"
            , plainTextBlock "Second item"
            , plainTextBlock "Third item"
            ])
