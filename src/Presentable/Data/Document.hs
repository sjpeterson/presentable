{-# LANGUAGE TupleSections #-}

module Presentable.Data.Document ( Document (..)
                                 , FormattedText (..)
                                 , Node (..)
                                 , ListItem (..)
                                 , ListNode (..)
                                 , Style (..)
                                 , plain
                                 , plainText
                                 ) where

-- |
-- Module      : Presentable.Data.Document
-- Copyright   : 2022 Stefan Peterson
-- License     : MIT
-- 
-- Maintainer  : Stefan Peterson <stefan.j.peterson@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Internal document type and subtypes.

import Data.Text ( Text )
import Data.List.NonEmpty ( NonEmpty )

-- | Document data type. A newtype for a non-empty list of nodes.
newtype Document = Document (NonEmpty Node)
    deriving (Eq, Show)

data Node = Heading FormattedText [Node]
          | Blockquote [Node]
          | Paragraph FormattedText
          | Art Text
          | Code Text
          | List ListNode
    deriving (Eq, Show)

newtype FormattedText = FormattedText [(Text, Style)]
    deriving (Eq, Show)

data Style = Style { styleBold :: Bool
                   , styleItalic :: Bool
                   , styleUnderline :: Bool
                   , styleStrikethrough :: Bool
                   }
    deriving (Eq, Show)

plain :: Style
plain = Style { styleBold = False
              , styleItalic = False
              , styleUnderline = False
              , styleStrikethrough = False
              }

plainText :: Text -> FormattedText
plainText = FormattedText . pure . (, plain)

data ListNode = UnorderedList [ListItem]
              | OrderedList [ListItem]
    deriving (Eq, Show)

data ListItem = ListItem Text (Maybe ListNode)
    deriving (Eq, Show)
