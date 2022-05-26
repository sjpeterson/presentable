{-# LANGUAGE OverloadedStrings #-}

module Presentable.Data.SlideshowSpec where

import Test.Hspec ( Spec, describe, it, shouldBe )

import Presentable.Data.Slideshow ( Copyright (..)
                                  , CopyrightYear (..)
                                  )

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
