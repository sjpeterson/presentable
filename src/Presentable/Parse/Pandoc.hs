{-# LANGUAGE OverloadedStrings #-}

module Presentable.Parse.Pandoc ( parseMarkdownSlideshow ) where

import Data.Text ( Text )
import qualified Data.Text.IO as TIO
import Text.Pandoc ( Pandoc, PandocError, def, runPure, readMarkdown )

import Presentable.Data.Slideshow
    ( Slideshow )

type ParsingError = Text

parseMarkdown :: FilePath -> IO (Either PandocError Pandoc)
parseMarkdown path = do
    document <- TIO.readFile path
    return $ runPure $ readMarkdown def document

fromPandoc :: Pandoc -> Either ParsingError Slideshow
fromPandoc = undefined

parseMarkdownSlideshow :: FilePath -> IO (Either ParsingError Slideshow)
parseMarkdownSlideshow path = do
    result <- parseMarkdown path
    return $ case result of
        Left _ -> Left "Pandoc failed to parse markdown"
        Right doc -> fromPandoc doc
