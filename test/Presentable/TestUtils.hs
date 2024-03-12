module Presentable.TestUtils where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

import Presentable.Data.Slideshow (
    BulletList (BulletList),
    BulletListItem (BulletListItem),
    SlideContent (BulletListContent),
 )
import Presentable.Data.TextBlock (plainTextBlock)

flatBulletList :: NonEmpty Text -> SlideContent
flatBulletList = BulletListContent . flatBulletList'

nestedBulletList :: NonEmpty (Text, Maybe (NonEmpty Text)) -> SlideContent
nestedBulletList = BulletListContent . nestedBulletList'

flatBulletList' :: NonEmpty Text -> BulletList
flatBulletList' = BulletList . fmap listItem
  where
    listItem = (flip BulletListItem) Nothing . plainTextBlock

nestedBulletList' :: NonEmpty (Text, Maybe (NonEmpty Text)) -> BulletList
nestedBulletList' = BulletList . fmap listItem
  where
    listItem (itemText, sublist) =
        BulletListItem (plainTextBlock itemText) (fmap flatBulletList' sublist)
