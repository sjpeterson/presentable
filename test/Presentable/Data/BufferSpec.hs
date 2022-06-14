{-# LANGUAGE OverloadedLists #-}

module Presentable.Data.BufferSpec where

import Test.Hspec ( Spec, describe, it, shouldBe )

import Presentable.Data.Buffer ( Buffer (..)
                               , bufferOf
                               , forwardUntil
                               , next
                               , prev
                               )

spec :: Spec
spec = do
    describe "bufferOf" $ do
        it "creates a buffer and start at the first item" $ do
            bufferOf [1..5] `shouldBe` Buffer 1 [2, 3, 4, 5] []
    describe "next" $ do
        it "takes one step ahead if there is a next item" $ do
            next (Buffer 1 [2, 3, 4, 5] []) `shouldBe`
                Buffer 2 [3, 4, 5] [1]
        it "remains in place if there are no more items" $ do
            next (Buffer 5 [] [4, 3, 2, 1]) `shouldBe`
                Buffer 5 [] [4, 3, 2, 1]
    describe "prev" $ do
        it "takes one step pack if there is a previous item" $ do
            prev (Buffer 5 [] [4, 3, 2, 1]) `shouldBe`
                Buffer 4 [5] [3, 2, 1]
        it "remains in place if there are no previous items" $ do
            prev (Buffer 1 [2, 3, 4, 5] []) `shouldBe`
                Buffer 1 [2, 3, 4, 5] []
    describe "forwardUntil" $ do
        it "stops before the first item that fulfills the condition" $ do
            forwardUntil (> 3) (Buffer 1 [2, 3, 4, 5] []) `shouldBe`
                Buffer 3 [4, 5] [2, 1]
        it "continues to the end if no item fulfills the condition" $ do
            forwardUntil (< 0) (Buffer 1 [2, 3, 4, 5] []) `shouldBe`
                Buffer 5 [] [4, 3, 2, 1]