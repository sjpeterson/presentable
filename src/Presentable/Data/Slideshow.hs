-- |
-- Module
-- Copyright
-- License
--
-- Presentation data types for presentable

module Presentable.Data.Slideshow where

import Data.Text ( Text, unpack )

type Title = Text

data Slideshow =
    Slideshow { slideshowCopyright :: Maybe Copyright
              , slideshowSlides :: [Slide]
              } deriving ( Show, Eq )

data Slide = TitleSlide Title (Maybe Text)
           | SingleContentSlide Title SlideContent
               deriving ( Show, Eq )

data SlideContent = BulletList [Text]
                  | NoContent
                      deriving (Show, Eq)

data Copyright =
    Copyright { copyrightAuthor :: Text
              , copyrightYear :: Maybe CopyrightYear
              } deriving ( Eq )

instance Show Copyright where
    show (Copyright author year) = "© " ++ case year of
        Nothing                  -> unpack author
        Just (SingleYear year')  -> unwords [ show year', unpack author ]
        Just (YearRange from to) -> unwords [ concat [show from, "-", show to]
                                            , unpack author
                                            ]

data CopyrightYear = SingleYear Int
                   | YearRange Int Int
                        deriving ( Show, Eq )

data Dimensions =
    Dimensions { dimensionsColumns :: Int
               , dimensionsRows :: Int
               } deriving ( Show, Eq )