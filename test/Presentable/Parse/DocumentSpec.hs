{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Presentable.Parse.DocumentSpec where

import Data.Either ( isLeft )
import Test.Hspec ( Spec, describe, it, shouldBe, shouldSatisfy )

import Presentable.Data.Document
    ( Document (..)
    , Node (..)
    , FormattedText (..)
    , Style (..)
    , plain
    )
import Presentable.Parse.Document ( parseMarkdown )

spec :: Spec
spec = do
    describe "parseMarkdown" $ do
        it "fails to parse an empty string" $ do
            parseMarkdown "" `shouldSatisfy` isLeft
        it "parses a single line as a paragraph" $ do
            parseMarkdown "test line" `shouldBe`
                Right (Document [Paragraph $ FormattedText [("test line", plain)]])
