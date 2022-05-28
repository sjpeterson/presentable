{-# LANGUAGE OverloadedStrings #-}

module Presentable.UI.Brick.Draw where

import Data.List.NonEmpty ( NonEmpty )
import qualified Data.List.NonEmpty as NE
import Data.Text ( Text )
import Brick
    ( App ( App
          , appDraw
          , appChooseCursor
          , appHandleEvent
          , appStartEvent
          , appAttrMap
          )
    , AttrMap
    , AttrName
    , BrickEvent ( VtyEvent )
    , EventM
    , Next
    , Padding ( Max, Pad )
    , Widget
    , (<+>)
    , attrMap
    , attrName
    , continue
    , defaultMain
    , emptyWidget
    , fg
    , halt
    , hLimit
    , neverShowCursor
    , padAll
    , padBottom
    , padLeft
    , padRight
    , str
    , txt
    , txtWrap
    , vBox
    , vLimit
    , withAttr
    )
import Brick.Widgets.Center as C
import Lens.Micro ( (&), (.~), (^.), over, set )

import Presentable.App.Env ( AppEnv ( slideshow ) )
import Presentable.App.State ( AppState
                             , appStateColumns
                             , appStateSlidesBuffer
                             )
import Presentable.Data.Buffer ( bufferCurrent )
import Presentable.Data.Slideshow ( InlineTextTag ( PlainText )
                                  , Slide ( SingleContentSlide , TitleSlide )
                                  , SlideContent ( BulletList, NoContent )
                                  , Slideshow ( slideshowCopyright
                                              , slideshowSlides
                                              )
                                  , TaggedText
                                  , TextBlock
                                  )
import Presentable.Process.Slideshow ( wrapRelaxedAt )
import Presentable.UI.Brick.Attributes ( bulletAttr, errorAttr, titleAttr )

type Name = ()

-- | Draw the application state
drawUI :: AppEnv -> AppState -> [Widget Name]
drawUI appEnv appState =
    [ C.center $ hLimit 80 $ vLimit 24 $ (padAll 1) inner ]
  where
    inner = vBox [ padBottom Max $ padRight Max $ slide
                 , copyrightNotice
                 ]
    slide = either drawError (drawSlide columns) $
        bufferCurrent <$> appState ^. appStateSlidesBuffer
    columns = appState ^. appStateColumns
    copyrightNotice = case (slideshowCopyright $ slideshow appEnv) of
        Nothing        -> emptyWidget
        Just copyright -> padLeft Max $ str $ show copyright

-- | Draw an error
drawError :: Text -> Widget Name
drawError = C.hCenter . withAttr errorAttr . txt

-- | Draw a slide
drawSlide :: Int -> Slide -> Widget Name
drawSlide _ (TitleSlide title subtitle) = case subtitle of
    Nothing -> titleWidget
    Just s  -> C.center $ vBox [ padBottom (Pad 1) titleWidget
                               , C.hCenter $ txt s
                               ]
  where
    titleWidget = C.hCenter $ withAttr titleAttr $ txt title
drawSlide columns (SingleContentSlide title content) = padBottom Max $
    vBox [ padBottom (Pad 1) $ withAttr titleAttr $ txt title
         , drawContent columns content
         ]

-- | Draw slide contents
drawContent :: Int -> SlideContent -> Widget Name
drawContent _       NoContent          = emptyWidget
drawContent columns (BulletList items) = padRight Max $
    vBox $ map (drawBulletListItem columns) items

-- | Draw a bullet list item
drawBulletListItem :: Int -> TextBlock -> Widget Name
drawBulletListItem columns tb =
    withAttr bulletAttr (txt "• ") <+> drawTextBlock (columns - 2) tb

-- | Draw a text block wrapped at the specified width.
drawTextBlock :: Int -> TextBlock -> Widget Name
drawTextBlock columns tb = vBox $ map drawLine $ wrapRelaxedAt columns tb

-- | Draw a single line of text
drawLine :: NonEmpty TaggedText -> Widget Name
drawLine line = drawTaggedText t <+> case ts of
    Nothing    -> emptyWidget
    (Just ts') -> txt " " <+> drawLine ts'
  where
    (t, ts) = NE.uncons line

-- | Draw a single tagged text element
drawTaggedText :: TaggedText -> Widget Name
drawTaggedText (s, PlainText) = txt s
