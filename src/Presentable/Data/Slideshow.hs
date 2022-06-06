{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Presentable.Data.Slideshow where

import Data.List.NonEmpty ( NonEmpty ( (:|) ) )
import Data.Text ( Text, unpack )

-- | A type alias for the slideshow title.
type Title = Text

-- | Slideshow data type. A slideshow consists of optional copyright information
-- and a non-empty list of slides.
data Slideshow = Slideshow
    { slideshowCopyright :: Maybe Copyright
    , slideshowSlides :: NonEmpty Slide
    } deriving (Eq, Show )

-- | Slide data type.
data Slide = TitleSlide Title (Maybe Text)
           | SingleContentSlide Title SlideContent
    deriving (Eq, Show)

-- | Slide content data type.
data SlideContent = BulletList (NonEmpty TextBlock)
                  | NoContent
    deriving (Eq, Show)

-- | Copyright information.
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

-- | Data type for either a year or a year range.
data CopyrightYear = SingleYear Int
                   | YearRange Int Int
                        deriving (Eq, Show)

-- | A newtype for blocks of text that must not be split. A non-empty list of
-- tagged inline text parts.
newtype TextBlock = TextBlock
    { unTextBlock :: NonEmpty TaggedText
    } deriving (Eq, Show)

-- | A type alias for some inline text and a tag describing its type.
type TaggedText = (Text, InlineTextTag)

-- | The different types of inline text.
data InlineTextTag = PlainText
    deriving (Eq, Show)

-- | Create a TextBlock from some Text.
plainTextBlock :: Text -> TextBlock
plainTextBlock = TextBlock . (:| []) . (flip (,) PlainText)
