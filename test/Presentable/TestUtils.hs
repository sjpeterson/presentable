module Presentable.TestUtils where

import Data.List.NonEmpty ( NonEmpty )
import Data.Text ( Text )

import Presentable.Data.Slideshow ( BulletList ( BulletList )
                                  , BulletListItem ( BulletListItem )
                                  , SlideContent ( BulletListContent ))
import Presentable.Data.TextBlock ( plainTextBlock )

flatBulletList :: NonEmpty Text -> SlideContent
flatBulletList = BulletListContent . BulletList . fmap listItem
  where
    listItem = (flip BulletListItem) Nothing . plainTextBlock
