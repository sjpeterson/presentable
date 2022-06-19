{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Presentable.Data.SlideshowSpec where

import Test.Hspec ( Spec, describe, it, shouldBe )

import Presentable.Data.Block ( wrappedHeightAt )
import Presentable.Data.Slideshow ( BulletListItem (..)
                                  , Copyright (..)
                                  , CopyrightYear (..)
                                  )
import Presentable.Data.TextBlock ( plainTextBlock )

import Presentable.TestUtils ( flatBulletList' )

spec :: Spec
spec = do
    describe "Show Copyright" $ do
        it "shows only copyright holder when year is Nothing" $ do
            show (Copyright "Example C. Holder" Nothing) `shouldBe`
                "© Example C. Holder"
        it "shows a single year before the copyright holder" $ do
            show (Copyright "Example C. Holder" (Just $ SingleYear 2022))
                `shouldBe` "© 2022 Example C. Holder"
        it "shows a year range with a dash and no spaces" $ do
            show (Copyright "Example C. Holder" (Just $ YearRange 2020 2022))
                `shouldBe` "© 2020-2022 Example C. Holder"
    describe "Block BulletListItem wrapped height" $ do
        it "accounts for bullet character" $ do
            wrappedHeightAt 11 testBulletListLeafItem `shouldBe` Right 1
            wrappedHeightAt 10 testBulletListLeafItem `shouldBe` Right 2
        it "includes children" $ do
            wrappedHeightAt 20 testBulletListNodeItem `shouldBe` Right 3
        it "accounts for sublist indentation" $ do
            wrappedHeightAt 16 testBulletListNodeItem `shouldBe` Right 3
            wrappedHeightAt 15 testBulletListNodeItem `shouldBe` Right 4

  where
    testBulletListLeafItem = BulletListItem (plainTextBlock "Item text") Nothing
    testBulletListNodeItem = BulletListItem
        (plainTextBlock "Item text")
        (Just $ flatBulletList' ["First child", "Second child"])
