{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Presentable.Parse.Document where

import Data.Text ( Text )

import Presentable.Data.Document ( Document (..), Node (..), plainText )

newtype ParsingError = ParsingError Text
    deriving (Eq, Show)

parseMarkdown :: Text -> Either ParsingError Document
parseMarkdown "" = Left $ ParsingError "Empty document"
parseMarkdown s = Right $ Document [Paragraph $ plainText s]
