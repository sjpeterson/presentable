{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Presentable.Process.SlideshowSpec where

import Data.Either ( isLeft )
import Data.List.NonEmpty ( NonEmpty ( (:|) ) )

import Test.Hspec ( Spec, describe, it, shouldBe, shouldSatisfy )

import Presentable.Data.Geometry ( Rect ( Rect ) )
import Presentable.Data.Slideshow ( Slide ( SingleContentSlide, TitleSlide )
                                  , SlideContent ( BulletListContent )
                                  )
import Presentable.Data.TextBlock ( TextBlock ( TextBlock ) , plainTextBlock )
import Presentable.Process.Slideshow ( fitTo, zipValues )

import Presentable.TestUtils ( flatBulletList )


spec :: Spec
spec = do
    describe "zipValues" $ do
        it "pairs the first slide with position 0" $ do
            zipValues [testTitleSlide] `shouldBe` [(testTitleSlide, 0)]
            zipValues [testBulletListSlide] `shouldBe`
                [(testBulletListSlide, 0)]
        it "zips with a cumulative sum of values" $ do
            zipValues [testTitleSlide, testBulletListSlide, testBulletListSlide]
                `shouldBe` [ (testTitleSlide, 0)
                           , (testBulletListSlide, 1)
                           , (testBulletListSlide, 6)
                           ]
    describe "fitTo" $ do
        it "leaves a small enough title slide intact" $ do
            fitTo (Rect 14 4) 0 [testTitleSlide] `shouldBe`
                Right [testTitleSlide]
        it "fails if a title slide is too large" $ do
            fitTo (Rect 14 3) 0 [testTitleSlide] `shouldSatisfy` isLeft
        it "deducts footer height on both sides of a title slide" $ do
            fitTo (Rect 14 7) 2 [testTitleSlide] `shouldSatisfy` isLeft
        it "splits a long bullet list leaving items intact" $ do
            fitTo (Rect 14 7) 0 [testBulletListSlide] `shouldBe`
                Right [ SingleContentSlide
                            "Slide Title"
                            (flatBulletList [ "First item"
                                            , "Second item"
                                            , "Third item"
                                            ])
                      , SingleContentSlide
                            "Slide Title"
                            (flatBulletList [ "Fourth item"
                                            , "Fifth item"
                                            ])
                      ]
        it "deducts footer height at one end of a bullet list slide" $ do
            fitTo (Rect 14 9) 2 [testBulletListSlide] `shouldBe`
                Right [ SingleContentSlide
                            "Slide Title"
                            (flatBulletList [ "First item"
                                            , "Second item"
                                            , "Third item"
                                            ])
                      , SingleContentSlide
                            "Slide Title"
                            (flatBulletList [ "Fourth item"
                                            , "Fifth item"
                                            ])
                      ]
        it "fails if a bullet list item is too large to fit" $ do
            fitTo (Rect 14 7) 0 [testLongItemBulletListSlide] `shouldSatisfy`
                isLeft
        it "fails if the first bullet list item is too large to fit" $ do
            fitTo (Rect 14 7) 0 [testLongFirstItemBulletListSlide]
                `shouldSatisfy` isLeft
  where
    testTitleSlide = TitleSlide "Presentation" (Just "With a subtitle")
    testBulletListSlide = SingleContentSlide
        "Slide Title"
        (flatBulletList [ "First item"
                        , "Second item"
                        , "Third item"
                        , "Fourth item"
                        , "Fifth item"
                        ])
    testLongItemBulletListSlide = SingleContentSlide
        "Slide Title"
        (flatBulletList [ "First item"
                        , "A long item that does not fit in the space given"
                        , "Third item"
                        ])
    testLongFirstItemBulletListSlide = SingleContentSlide
        "Slide Title"
        (flatBulletList [ "A long item that does not fit in the space given"
                        , "Second item"
                        , "Third item"
                        ])
