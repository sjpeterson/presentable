-- |
-- Module
-- Copyright
-- License
--
-- Presentation data types for presentable

module Presentable.Data.Presentation where

import Data.Text ( Text )

data Presentation = 
    Presentation { presentationTitle :: Text
                 , presentationSubTitle :: Maybe Text
                 , presentationCopyright :: Maybe Copyright
                 , presentationSlides :: [Slide]
                 } deriving ( Show, Eq )

data Slide =
    Slide { slideTitle :: Text
          , slideContent :: SlideContent
          } deriving ( Show, Eq )

data SlideContent = BulletList [Text]
                  | NoContent
                        deriving (Show, Eq)

data Copyright =
    Copyright { copyrightAuthor :: Text
              , copyrightYear :: Maybe CopyrightYear
              } deriving ( Show, Eq )

data CopyrightYear = SingleYear Int
                   | YearRange Int Int
                        deriving ( Show, Eq )
