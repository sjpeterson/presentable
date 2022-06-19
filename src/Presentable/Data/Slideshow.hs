{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Presentable.Data.Slideshow where

import Control.Monad ( liftM2 )
import Data.List.NonEmpty ( NonEmpty )
import Data.Text ( Text, unpack )

import Presentable.Data.Block ( Block ( wrappedHeightAt ) )
import Presentable.Data.TextBlock ( TextBlock )

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
data SlideContent = BulletListContent BulletList
                  | NoContent
    deriving (Eq, Show)

-- | Bullet list data type. A newtype for a non-empty list of bullet list items.
newtype BulletList = BulletList
    { unBulletList :: NonEmpty BulletListItem
    } deriving (Eq, Show)

-- | Bullet list item type. Consists of an item text and an optional sublist.
data BulletListItem = BulletListItem TextBlock (Maybe BulletList)
    deriving (Eq, Show)

instance Block BulletListItem where
    wrappedHeightAt c (BulletListItem itemText sublist) = liftM2 (+)
        (wrappedHeightAt (c - 2) itemText)
        (sublistHeight)
      where
        sublistHeight = case sublist of
            Nothing       -> Right 0
            Just sublist' -> foldl f (Right 0) (unBulletList sublist')
        f err@(Left _) _           = err
        f (Right n)    sublistItem = fmap (n +) $
                                         wrappedHeightAt (c - 4) sublistItem

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

