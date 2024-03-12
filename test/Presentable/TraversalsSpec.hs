{-# LANGUAGE OverloadedLists #-}

module Presentable.TraversalsSpec where

import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Presentable.Traversals (mapExpandM, splitWhen)

spec :: Spec
spec = do
    describe "mapExpandM" $ do
        it "preserves order" $ do
            mapExpandM selfReplicate [1, 2, 3]
                `shouldBe` Right [1, 2, 2, 3, 3, 3]
        it "propagates failure" $ do
            mapExpandM selfReplicate [3, 2, 1, 0] `shouldSatisfy` isLeft
            mapExpandM selfReplicate [1, 0, 1] `shouldSatisfy` isLeft
            mapExpandM selfReplicate [0, 1, 2] `shouldSatisfy` isLeft
    describe "splitWhen" $ do
        it "splits from the start, preserving order" $ do
            splitWhen idNotZero (> 5) (+) id [1, 2, 3, 1, 4]
                `shouldBe` Right [[1, 2], [3, 1], [4]]
        it "fails if an individual item violates constraint" $ do
            splitWhen idNotZero (> 5) (+) id [6, 3, 2] `shouldSatisfy` isLeft
            splitWhen idNotZero (> 5) (+) id [3, 6, 2] `shouldSatisfy` isLeft
            splitWhen idNotZero (> 5) (+) id [3, 2, 6] `shouldSatisfy` isLeft
        it "fails if the value cannot be computed for any individual item" $ do
            splitWhen idNotZero (> 5) (+) id [0, 3, 2] `shouldSatisfy` isLeft
            splitWhen idNotZero (> 5) (+) id [3, 0, 2] `shouldSatisfy` isLeft
            splitWhen idNotZero (> 5) (+) id [3, 2, 0] `shouldSatisfy` isLeft
  where
    selfReplicate x
        | x <= 0 = Left "Nope"
        | otherwise = Right $ x :| replicate (x - 1) x
    idNotZero x = if x == 0 then Left 0 else Right x
