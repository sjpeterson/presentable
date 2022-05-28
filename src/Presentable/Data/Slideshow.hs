{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module
-- Copyright
-- License
--
-- Presentation data types for presentable

module Presentable.Data.Slideshow where

import Data.List.NonEmpty ( NonEmpty ( (:|) ) )
import Data.Text ( Text, unpack )
import qualified Data.Text as T

type Title = Text

data Slideshow = Slideshow
    { slideshowCopyright :: Maybe Copyright
    , slideshowSlides :: NonEmpty Slide
    } deriving (Eq, Show )

data Slide = TitleSlide Title (Maybe Text)
           | SingleContentSlide Title SlideContent
           | ErrorSlide Text
    deriving (Eq, Show)

data SlideContent = BulletList [TextBlock]
                  | NoContent
    deriving (Eq, Show)

data Copyright = Copyright
    { copyrightAuthor :: Text
    , copyrightYear :: Maybe CopyrightYear
    } deriving Eq

instance Show Copyright where
    show (Copyright author year) = "© " ++ case year of
        Nothing                  -> unpack author
        Just (SingleYear year')  -> unwords [ show year', unpack author ]
        Just (YearRange from to) -> unwords [ concat [show from, "-", show to]
                                            , unpack author
                                            ]

data CopyrightYear = SingleYear Int
                   | YearRange Int Int
                        deriving (Eq, Show)

newtype TextBlock = TextBlock
    { unTextBlock :: NonEmpty TaggedText
    } deriving (Eq, Show)

type TaggedText = (Text, InlineTextTag)

data InlineTextTag = PlainText
    deriving (Eq, Show)

plainTextBlock :: Text -> TextBlock
plainTextBlock = TextBlock . (:| []) . (flip (,) PlainText)
